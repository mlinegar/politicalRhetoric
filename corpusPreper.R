# text loading and pre-processing

library(tidyverse)
library(devtools)
library(mallet)
install_github("mlinegar/litMagModelling")
library(litMagModelling)
library(tidytext)
library(tm)
# increase Java's available memory size
options(java.parameters = "-Xmx4g")
# some brief cleaning
library(lubridate)
library(dplyr)

collectedSpeeches <- read_csv("thesisSpeeches.csv")
collectedSpeeches$X1 <- NULL
collectedSpeeches <- mutate(collectedSpeeches, author_speech_id = paste(author, rownames(collectedSpeeches), sep = "_"))
collectedSpeeches$text <- tolower(collectedSpeeches$text)
collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, "\n", " ")
collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, "<br />", " ")
# date filter
collectedSpeeches <- collectedSpeeches %>% filter(date < mdy("11/08/2016") & date >= mdy("07/01/2016"))

# create metadata to work with later
collectedSpeeches.metadata <- collectedSpeeches %>% select(author_speech_id, author, date, title, source)
write.csv(collectedSpeeches.metadata, "collectedSpeeches.metadata.csv")
# may want to selectively replace punctuation: replace . as " " if word.length >5 (ex ".")
# else replace . ""
# code with help from stackoverflow user G. Grothendieck, here: 
# https://stackoverflow.com/questions/46394301/replacing-punctuation-in-string-in-different-ways-by-word-length-in-r/46395170#46395170
library(gsubfn)
# explanation:
# looks for a delimiter, up to 6 non-space characters and a delimiter 
# for any such match it runs the anonymous function specified in formula notation 
# in the second argument of gsubfn. That anonymous function removes any periods in the match. 
# In what is left the gsub replaces each period with a space.
pat <- "(?<=^| )(\\S{1,6})(?=$| )"
collectedSpeeches$text <- gsub("[.]", " ", gsubfn(pat, ~ gsub("[.]", "", ..1), collectedSpeeches$text, perl = TRUE))



# just doing this crudely for now
# doing this for apostrophes eventually gives a conjunction topic, which isn't ideal
# any better idea?
collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, "[\'\"]", "")
collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, "[[:punct:]]", " ")
collectedSpeeches$text <- removePunctuation(collectedSpeeches$text)

# should probably do full name parsing first to prevent doubling of candidates when their
# first and last name appears
# trying to do this below with twotokenlist
# mapping usa/united states( of america)? to unitedstates
# isis, islamic state, and isil map to isis because they both appear relatively infrequently
twotokenlist <- read.csv("twotokenlist2.csv", stringsAsFactors = FALSE)
for(phrase in 1:nrow(twotokenlist)){
  collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, twotokenlist$from[phrase], twotokenlist$to[phrase])
}

# big problem is in interviews, when candidates appear to talk about themselves a lot because their 
# name is mentioned on the transcript. Gotta fix that.
# because the twotokenlist (above) should have replaced all first/last/combo names with a single string
# we *should* be able to proceed in this simple manner

collectedSpeeches <- collectedSpeeches %>% mutate(text = str_replace_all(text, 
                                                                         removePunctuation(tolower(str_replace_all(author, " ", ""))), " "))

# a related issue is that people talk about their opponents by name, which (especially for more than one election) is undesirable
# this replaces named opponents by just "opponent"
allauthors <- removePunctuation(tolower(str_replace_all(unique(collectedSpeeches$author), " ", "")))
# let's replace all instances of a politician talking about someone else by them saying "opponent"
collectedSpeeches <- collectedSpeeches %>% mutate(text = str_replace_all(text, paste(allauthors[!(allauthors %in% author)], collapse = "|"), "opponent"))

# need to stem/lemmatize the corpus
# THIS APPEARS BROKEN - NOT RECOMMENDED
# library(textstem)
# collectedSpeeches$text <- lemmatize_strings(collectedSpeeches$text)
# instead I'll try with koRpus
library(koRpus)
texts <- treetag(collectedSpeeches$text, treetagger = "manual", format = "obj", TT.tknz = FALSE, lang = "en",
                 TT.options = list(path="./TreeTagger", preset="en"))
textsTreed <- data.frame(texts@TT.res)
textsTreed <- textsTreed %>% filter(lemma!="<unknown>" & token!=lemma) %>% select(token, lemma) %>% unique()
textsTreed$lemma <- tolower(textsTreed$lemma)

for(i in 1:nrow(textsTreed)){
  collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, paste0(" ", textsTreed$token[i], " "), paste0(" ", textsTreed$lemma[i], " "))
}

# there appear to be some errors coming from this lemmatizer (as there will be in any such tool)
# I'll manually fix these
# contacted the maintainer and the errors should no longer appear
# lemmaerrors <- read.csv("lemmaerrors.csv", stringsAsFactors = FALSE)
# for(phrase in 1:nrow(lemmaerrors)){
#   collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, lemmaerrors$from[phrase], lemmaerrors$to[phrase])
# }
# do this a second time because some names get caught up in this
# twotokenlist <- read.csv("twotokenlist2.csv", stringsAsFactors = FALSE)
# for(phrase in 1:nrow(twotokenlist)){
#   collectedSpeeches$text <- str_replace_all(collectedSpeeches$text, twotokenlist$from[phrase], twotokenlist$to[phrase])
# }

# and now tokenize!
# have to include "conjurer" because for some reason lemmatize_strings replaces "also" with "conjurer"
# I've reached out to get it fixed
# some of these I'm not sure I should include - they appear to be the way trump talks
# words left in: mosul, detroit - not taken out b/c they're not places these speeches happened, more campaign issues
custom_stopwords <- c("applause", "mr", "will", "sen", "can", "go", "get", "say", "much",
                      "cheer", "applause", "cd", "boo", "like", "think", "just", "applpause",
                      "crosstalk", "matthews", "laughter", "jansing", "burnett", "tapper", 
                      "cheering", "cheers", "bash", "camerota", "dickerson", "mitchell",
                      "inaudible", "maddow", "anderson", "cooper", "um", "megan", "kelly",
                      "chris", "hayes", "shepard", "smith", "moderator", "todd", "chuck", 
                      "th", "youre", "hell", "hes", "shes", "ive", "uh", "weve", "theyre",
                      "theres", "didn", "didnt", ", illl", "youre", "whats", "doesnt", 
                      "doesn", "theyve", "cuomo", "youve", "joe", "michael", "wont", "week",
                      "ago", "wasn", "heres", "lot", "patrick", "murphy", "wouldnt", "guy",
                      "percent", "conjurer", "issue", "understand", "question", "stand",
                      "candidate", "election", "ill", "debate", "answer", "tonight", "jackson",
                      "john", "schultz", "senator", "governor", "barbara", "stephanopoulos",
                      "th", "nice", "gonna", "isnt", "rudy", "al", "tremendous",
                      "agendum", "st", "numb", "smart", "hed",
                      "johnson", "elizabeth", "shed", "speak", "na", "dade",
                      "even", "keep", "look", "thing", "never", "happen", "tell", "city", "state", "need", "fight",
                      "newhampshire", "florida", "pennsylvania", "iowa", "miami", "northcarolina", "michigan", "ohio", "indiana",
                      "thousand", "knock", "theyll", "ready", "totally", "reince", "amaze", "unbelievable", "tomorrow",
                      "tim", "lauren", "marian", "anastasia", "freia", "maggie", "colin", "anne", "zianna", "excite", "hand",
                      "ph", "audience", "speech", "convention", "virginia", "crowd",
                      "beautiful", "story", "watch", "register", "double", "stake", "matter", "feel", "party",
                      "mention", "yesterday", "newyork", "massive", "ahead", "senate",  "reason", "anymore",
                      "hour", "night", "line", "stay", "youin", "miss",
                      "ever", "incredible", "believe", "horrible", "tough", "mean", "call", "kind",
                      "presidential", "candidate", "democratic", "republican", "president", "campaign",
                      "forget", "set", "react", "listen", "hold", "deeply", "play", "team", "treat", "level", "entire", 
                      "imagine", "half", "forward", "pueblo", "colorado", "carr", "philadelphia", 
                      "minute", "chant", "decade", "potential", letters, "one", "two", "three", "four", 
                      "five", "six", "seven", "eight", "nine", "ten", "month", "card", "ad", "re", "ve", "ll", "ok", 
                      "okay", "probably", "really", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", 
                      "eighth", "ninth", "tenth", "audio", "video", "unknown", "clip",
                      "sure", "everyone", "everybody", "talk", "hear",
                      "always", "bring", "give", "big", "nation", "unitedstates", "quote", "little", "high", 
                      "front", "maybe", "secretary", "still", "title", "vote", "today", "leave", "north", "south", "carolina", "york", 
                      "donaldtrump", "hillaryclinton", "berniesanders", "mikepence", "timkaine", "tim", "hillary",
                      "donald", "bernie")
# want topics with clear REFERENTS
# trying to remove words with no direct referent
# makes no reference to any event, entity, process, idea and so is unable to carry meaning in isolation
tm_stopwords <- str_replace_all(stopwords(), "[\'\"]", "")
tm_stopwords <- c(tm_stopwords, stopwords())


collectedSpeeches.tidy <- collectedSpeeches %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% custom_stopwords) %>%
  filter(!word %in% tm_stopwords)

# anti_join not working for me for some reason
# anti_join(custom_stopwords) %>%
# anti_join(stop_words) 


# calculate tf-idf
collectedSpeeches.tfidf <- collectedSpeeches.tidy %>%
  count(author_speech_id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, author_speech_id, n) %>%
  mutate(docProp = 1/exp(idf))
# invert idf:
# IDF(t) = log_e(Total number of documents / Number of documents with term t in it)
# so Number of documents with term t in it/Total number of documents = 1/e^IDF(t)

wordsToRemove.tfidf <- collectedSpeeches.tfidf %>%
  filter(docProp > .95 | docProp <= 4/nrow(collectedSpeeches)) %>% 
  transmute(word = word, docProp = docProp) %>%
  unique()
# 
# # count number of documents each word appears in
# collectedSpeeches.wordcounts <- collectedSpeeches.tidy %>%
#   select(word, author_speech_id) %>%
#   group_by(author_speech_id) %>%
#   unique() %>%
#   mutate(doccount = n(), docprop = doccount/nrow(collectedSpeeches)) %>%
#   arrange(desc(docprop)) %>%
#   select(-author_speech_id) %>%
#   unique()

# just manually removing certain words that seem important - will return to this later
#wordsToRemove.tfidf <- wordsToRemove.tfidf %>%
#  filter(!(word %in% c("job", "care", "system", "build", "life", "government")))

collectedSpeeches.tidy <- collectedSpeeches.tidy %>%
  anti_join(wordsToRemove.tfidf)

# and now stitch everything back together so we can run it with MALLET!
# much of this code was inspired by Text Mining with R: A Tidy Approach by Julia Silge and David Robinson

collectedSpeeches.collapsed <- collectedSpeeches.tidy %>%
  mutate(word = str_replace(word, "'", ""), id = author_speech_id) %>%
  group_by(author_speech_id) %>%
  summarize(text = paste(word, collapse = " "), date = first(date), id = first(id), author = first(author))

# have to do this because at some point the text column became a factor apparently
collectedSpeeches.collapsed <- data.frame(collectedSpeeches.collapsed, stringsAsFactors = FALSE)
processedSpeeches <- collectedSpeeches.collapsed
write.csv(processedSpeeches, "processedSpeeches.csv")
