# this installs speeches from three sources: The American Presidency Project by UCSB, The Grammar Lab, and Hillary Clinton's official website (or at least an archive of it)
# first, Clinton's speeches
devtools::install_github("hrbrmstr/wayback")
library(wayback)
library(tidyverse)
library(XML)
library(xml2)
library(rvest)
library(stringr)
library(lubridate)
# hillary clinton speeches
# these have since been removed, but can be found through the Internet Archive's Wayback Machine
# this is made easier by using the wayback package
# I originally scraped these speeches on 9/5/2017, but these are from 11/9/2016


# probably worth loading from the
# http://webarchive.loc.gov/all/20161109022147/https://www.hillaryclinton.com/speeches/page/1/
# because this seems to lead to old/bad links - all days are the same
# so this function would be ideal, but doesn't work for some reason
# great example of how to use the wayback package, though!
getClintonSpeechLinks <- function(){
  clintonSpeechLinks <- vector()
  # last 3 pages won't work (which is why only going to 9)
  # but this doesn't matter - they were all uploaded Jan 31, 2016
  # so they're outside the scope of this project
  for(i in 1:9){
    url <- paste0("http://webarchive.loc.gov/all/20161109144808/https://www.hillaryclinton.com/speeches/page/", i, "/#main")
    pg <- xml2::read_html(url)
    html <- htmlTreeParse(pg, useInternalNodes = T)
    speechLinks <- xpathApply(html, "//a[@href]", xmlGetAttr, "href")
    speechLinks <- as.vector(unlist(speechLinks))
    speechLinks <- speechLinks[grepl("speeches/[a-z]", speechLinks) & !grepl("page", speechLinks)]
    speechLinks <- unique(speechLinks)
    clintonSpeechLinks <- c(clintonSpeechLinks, speechLinks)
  }
  clintonSpeechLinks <- unique(clintonSpeechLinks)
  clintonSpeechLinks
}

getClintonSpeechLinks <- function(){
  clintonSpeechLinks <- vector()
  for(i in 1:12){
    url <- paste0("http://webarchive.loc.gov/all/20161109022147/https://www.hillaryclinton.com/speeches/page/", i, "/")
    speechPage <- read_html(url)
    html <- htmlTreeParse(speechPage, useInternalNodes = T)
    speechLinks <- xpathApply(html, "//a[@href]", xmlGetAttr, "href")
    speechLinks <- as.vector(unlist(speechLinks))
    speechLinks <- speechLinks[grepl("speeches/[a-z]", speechLinks) & !grepl("page", speechLinks) & !grepl("tim-kaine", speechLinks)]
    speechLinks <- unique(speechLinks)
    clintonSpeechLinks <- c(clintonSpeechLinks, speechLinks)
  }
  clintonSpeechLinks
}

# we'll instead use this somewhat messier function
# because this data relies on archived data, it's sometimes not available
# instead, I use different archival dates to access data from various save attempts
# these can be identified by each "try". I never needed more than three tries for any speech
getClintonSpeeches <- function(speechLinks, firstTry = "", secondTry = "", thirdTry = ""){
  clintonSpeeches <- data.frame()
  getClintonSpeech <- function(speechLink){
    speechPage <- read_html(speechLink)
    author <- "Hillary Clinton"
    nodeTest <- speechPage %>% html_node(".date") %>% html_text()
    if(!is.na(nodeTest)){
      Sys.sleep(5)
      speechDate <- speechPage %>% html_node(".date") %>% html_text() %>% lubridate::mdy()
      speechText <- speechPage %>% html_nodes(".rich-text p") %>% html_text()
      speechText <- speechText %>% unlist() %>% paste(collapse = "")
      speechName <- speechPage %>% html_node(".page-title") %>% html_text()
      df <- data.frame(date = speechDate, author = author, title = speechName, text = speechText, stringsAsFactors = FALSE)
      df
    } else {
      # if error, page takes a while to load
      Sys.sleep(10)
      speechDate <- speechPage %>% html_node("time") %>% html_text() %>% lubridate::mdy()
      speechText <- speechPage %>% html_nodes(".s-wysiwyg p") %>% html_text()
      speechText <- speechText %>% unlist() %>% paste(collapse = "")
      speechName <- speechPage %>% html_node(".c-post-title") %>% html_text()
      df <- data.frame(date = speechDate, author = author, title = speechName, text = speechText, stringsAsFactors = FALSE)
      df
    }
  }
  for (speech in seq_len(length(speechLinks))) {
    url <- speechLinks[speech]
    # this is messy, but I'm not sure how to make R actually assign in a better way
    # when I tried the straightforward way I just keep getting the base URL back
    # this is likely due to a small syntax error on my part
    url2 <- str_replace(url, firstTry, secondTry)
    url3 <- str_replace(url, firstTry, thirdTry)
    # for debugging - getting 503 error
    tryCatch({
      speech <- getClintonSpeech(url)
    }, error = function(err){
      message(paste("Error scraping URL:", url))
      message(paste("Here's the original error message:", err))
      tryCatch({
        message(paste("Trying second URL option, replacing", firstTry, "with", secondTry))
        message(paste("Trying URL", url2))
        speech <- getClintonSpeech(url2)
      }, error = function(err){
        message(paste("Error scraping URL:", url2))
        message(paste("Here's the original error message:", err))
        message(paste("Trying third URL option, replacing", secondTry, "with", thirdTry))
        message(paste("Trying URL", url3))
        tryCatch({
          speech <- getClintonSpeech(url3)
        }, error = function(err){
          message("All three tries failed")
          message(err)
        })
        
      })
    })
    clintonSpeeches <- rbind(clintonSpeeches, speech)
  }
  clintonSpeeches <- unique(clintonSpeeches)
  clintonSpeeches
}

clintonSpeechLinks <- getClintonSpeechLinks()
# this one is removed but still shows up from above function
# this speech is from 2015 anyway, so it doesn't matter if it's left out
clintonSpeechLinks <- clintonSpeechLinks[clintonSpeechLinks!="http://webarchive.loc.gov/all/20161109082527/https://www.hillaryclinton.com/speeches/remarks-cedar-falls-iowa-advancing-small-businesses/"]
clintonSpeechLinks <- str_replace(clintonSpeechLinks, "/web/", "http://webarchive.loc.gov/all/")
# it appears that some speeches aren't available from the 11/09 dump
# but might be from the 11/03 dump - just need to replace part of the URL
# note that the last 6 digits of the date-code aren't fixed, so only operate on the date portion
clintonSpeeches <- getClintonSpeeches(clintonSpeechLinks, 
                                      firstTry = "20161109", secondTry = "20161103", thirdTry = "20161102")
clintonSpeeches$source <- "Clinton Official"
clintonSpeeches$author <- "Hillary Clinton"


# now, for speeches from The American Presidency Project by UCSB 

# first let's go through all the speeches of a given politician
findCandidateSpeeches <- function(candidateurl){
  candidate_page <- read_html(candidateurl)
  speech_name <- candidate_page %>% html_nodes(".listdate a") %>% html_text()
  speech_loc <- candidate_page %>% html_nodes(".listdate a") %>% html_attr("href")
  speech_date <- candidate_page %>% html_nodes(".listdate:nth-child(2)") %>% html_text()
  speech_giver <- candidate_page %>% html_nodes(".listdate:nth-child(1)") %>% html_text()
  candidate_speeches <- data.frame(author = speech_giver, date = speech_date, title = speech_name, url = speech_loc, stringsAsFactors = FALSE)
  candidate_speeches$url <- str_replace(candidate_speeches$url, "..", "")
  candidate_speeches$url <- paste0("http://www.presidency.ucsb.edu", candidate_speeches$url)
  candidate_speeches$text <- vector(length = nrow(candidate_speeches))
  
  candidate_speeches$text <- sapply(candidate_speeches$url, function(x) x %>% read_html() %>% html_node(".displaytext") %>% html_text(), USE.NAMES = FALSE)
  candidate_speeches
}
# example
# trump_speeches <- findCandidateSpeeches("http://www.presidency.ucsb.edu/2016_election_speeches.php?candidate=45&campaign=2016TRUMP&doctype=5000")
# and now aggregate up to make it search through candidates for a given year

findCandidatesSpeechesByElection <- function(electionyear, otherSpeeches=NULL, minedCandidates=NULL){
  electionUrl <- paste0("http://www.presidency.ucsb.edu/", electionyear, "_election.php")
  electionPage <- read_html(electionUrl)
  candidate_pages <- electionPage %>% html_nodes(".doctext a:nth-child(1)") %>% html_attr("href")
  candidate_pages <- paste0("http://www.presidency.ucsb.edu/", candidate_pages)
  
  ifelse(is.null(otherSpeeches), candidate_speeches <- data.frame(), candidate_speeches <- otherSpeeches)
  for(candidate in 1:length(candidate_pages)){
    candidate_speech <- findCandidateSpeeches(candidate_pages[candidate])
    candidate_speeches <- rbind(candidate_speeches, candidate_speech)
  }
  candidate_speeches <- unique(candidate_speeches)
  candidate_speeches
}
UCSBSpeeches2016 <- findCandidatesSpeechesByElection("2016")
UCSBSpeeches2016$source <- "UCSB"
UCSBSpeeches2016$url <- NULL
UCSBSpeeches2016$date <- mdy(UCSBSpeeches2016$date)
#candidate_speeches$speechtype <- "Election Speech"
#candidate_speeches$authortype <- "Candidate"
# can also loop through by using a for loop and setting otherSpeeches to be the old version
# I know this is inefficient, but it's older code

# finally, speeches from The Grammer Lab
# this link will download the corpus, which contains two folders of text files
# one for Clinton and one for Trump
# http://www.thegrammarlab.com/?nor-portfolio=corpus-of-presidential-speeches-cops-and-a-clintontrump-corpus
library(readtext)
clintonGrammarLabSpeeches <- readtext(paste0("./Clinton-Trump Corpus", "/Clinton/*.txt"))
clintonGrammarLabSpeeches$author <- "Hillary Clinton"
clintonGrammarLabSpeeches$date <- ymd(gsub("Clinton_|*.txt", "", clintonGrammarLabSpeeches$doc_id))

trumpGrammarLabSpeeches <- readtext(paste0("./Clinton-Trump Corpus", "/Trump/*.txt"))
trumpGrammarLabSpeeches$author <- "Donald Trump"
trumpGrammarLabSpeeches$date <- ymd(gsub("Trump_|*.txt", "", trumpGrammarLabSpeeches$doc_id))

theGrammarLabSpeeches <- rbind(clintonGrammarLabSpeeches, trumpGrammarLabSpeeches)
theGrammarLabSpeeches$doc_id <- NULL
theGrammarLabSpeeches$title <- gsub(".*<title=|>.*", "", theGrammarLabSpeeches$text)
theGrammarLabSpeeches$title <- gsub("[[:punct:]]", "", theGrammarLabSpeeches$title)
theGrammarLabSpeeches$source <- "The Grammar Lab"

collectedSpeeches <- rbind(clintonSpeeches, UCSBSpeeches2016)
collectedSpeeches <- rbind(collectedSpeeches, theGrammarLabSpeeches)
collectedSpeeches[which(collectedSpeeches$author=="Donald J. Trump"),]$author <- "Donald Trump"
collectedSpeeches <- collectedSpeeches %>% filter(author=="Hillary Clinton" | author=="Donald Trump")

collectedSpeeches <- collectedSpeeches %>% filter(author=="Hillary Clinton" | author=="Donald Trump") %>% 
  filter(ymd(date) > ymd("2016-06-01"))

collectedSpeeches %>% filter(author=="Hillary Clinton" | author=="Donald Trump") %>%
  group_by(source, author) %>%
  summarise(total = n())

# optional (sort of): 
# need to check to see if any speeches are duplicates between the three sources
# doing this by following the RNewsflow vignette here:
# https://cran.r-project.org/web/packages/RNewsflow/vignettes/RNewsflow.html
library(RNewsflow)
library(RTextTools)

dtm <- RTextTools::create_matrix(textColumns = collectedSpeeches$text, minWordLength = 1, 
                                 stemWords = TRUE, removeNumbers = TRUE, removeStopwords = FALSE)
rownames(dtm) <- paste0("Document_", 1:nrow(dtm))
meta <- data.frame(document_id = paste0("Document_", 1:nrow(dtm)), date = collectedSpeeches$date, author = collectedSpeeches$author, source = collectedSpeeches$source)
#tdd = term.day.dist(dtm, meta)
#select_terms = tdd$term[tdd$days.entropy.norm <= 0.3]
#dtm = dtm[,select_terms]
dtm = weightTfIdf(dtm)
# as.numeric(max(collectedSpeeches$date) - min(collectedSpeeches$date)) * 24
g = newsflow.compare(dtm, meta,
                     hour.window = c(0,240), 
                     min.similarity = 0.6)
# vertices (or nodes) of this network represent documents
v = get.data.frame(g, 'vertices')
# The edges (or ties) represent the similarity of documents, 
# and the similarity score and time difference are stored as edge attributes
e = get.data.frame(g, 'edges')
# manual checking of the candidates resulted in the removal of these speeches:
# looks like almost everything above .602 is repeated (everything but two, which I exclude)
toRemove <- e %>% filter(weight > .602 & to!="Document_113" & hourdiff <= 48) %>% 
  mutate(to = as.numeric(str_replace(to, "Document_", ""))) %>% select(to)
toRemove <- unique(toRemove$to)

collectedSpeeches <- collectedSpeeches[-toRemove,]
# 90 total speeches removed as duplicates

write.csv(collectedSpeeches, "thesisSpeeches.csv")

# where do all the non-removed speeches come from?
speechSourceBreakown <- collectedSpeeches %>% filter(author=="Hillary Clinton" | author=="Donald Trump") %>%
  filter(ymd(date) >= ymd("2016-07-01")) %>%
  mutate(source = str_replace_all(source, "UCSB", "The American Presidency Project")) %>%
  mutate(source = str_replace_all(source, "Clinton Official", "hillaryclinton.com")) %>%
  group_by(source, author) %>%
  summarise(total = n())
speechSourceBreakown <- speechSourceBreakown %>% spread(author, total)
speechSourceBreakown
#library(xtable)
#print(xtable(speechSourceBreakown))
