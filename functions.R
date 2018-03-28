# this contains all the functions needed in to run the code included in this project
# at some point I may turn this into a (small) package, but for now it makes more sense to just run this file

# make corpus

makeCorpus <- function(processedCorpus, ntopics, dateFilter=NULL, metaData = NULL, metaDataJoinByCorpusCol = NULL, metaDataJoinByMetaDataCol = NULL, textcolname = "text", burnIn = 5000, numRuns = 20000, optFreq = 50, nlabelwords = 7){
  # dateFilter should be text or left blank
  stopifnot((is.null(dateFilter) | is.character(dateFilter)), is.numeric(ntopics))
  require(dplyr)
  require(litMagModelling)
  require(lubridate)
  if(!is.null(dateFilter)) {
    processedCorpus <- processedCorpus %>% filter(eval(parse(text = dateFilter)))
  }
  # create an empty file of "stopwords"
  file.create(empty_file <- tempfile())
  topic.model <- make_model(processedCorpus, n.topics = ntopics, textcolname = textcolname, stopListFile = empty_file, burnIn = burnIn, numRuns = numRuns, optFreq = optFreq)
  doc.topics <- mallet.doc.topics(topic.model, normalized = TRUE, smoothed = TRUE)
  topic.words <- mallet.topic.words(topic.model, normalized = TRUE, smoothed = TRUE)
  doc.topics.tidy <- tidy(topic.model, matrix = "gamma")
  # optional join with metadata
  if(!is.null(metaData)){
    ifelse((!is.null(metaDataJoinByCorpusCol) & !is.null(metaDataJoinByMetaDataCol)), 
           doc.topics.tidy <- doc.topics.tidy %>% left_join(metaData, by = eval(parse(text = paste0('c("', metaDataJoinByCorpusCol, '" = "', metaDataJoinByMetaDataCol, '")')))),
           doc.topics.tidy <- doc.topics.tidy %>% left_join(metaData))
  }
  topic.words.tidy <- tidy(topic.model, matrix = "beta")
  # labels
  topic.labels <- topic.words.tidy %>% 
    group_by(topic) %>%
    top_n(nlabelwords, beta) %>%
    summarize(top_words = paste(term, collapse = " "))
  # label all the topics
  doc.topics.tidy <- doc.topics.tidy %>% left_join(topic.labels)
  topic.words.tidy <- topic.words.tidy %>% left_join(topic.labels)
  doc.topics.tidy
}

# example

# testCorupsAug <- makeCorpus(processedSpeeches, 10, dateFilter = "month(date) > 8", 
#                             metaData = modernPoliticalAddresses.metadata, 
#                             metaDataJoinByCorpusCol = "document",
#                             metaDataJoinByMetaDataCol = "author_speech_id")


# convert daily obs to weekly obs
makeWeekly <- function(df){
  df %>% mutate(date = ymd(date), 
                week_start = floor_date(date, unit = "week"),
                # need to add 7 hours b/c for some reason off by 7 hours
                week_end = as.Date(ceiling_date(as.POSIXct(date + hours(7)), unit = "week", change_on_boundary = TRUE))) %>% 
    group_by(week_end, topic, author) %>%
    summarise(week_mean = mean(gamma), week_var = var(gamma), week_total = n(),
              week_start = unique(week_start)) %>%
    replace_na(list(week_var = 0))
}

makeWeeklyPolls <- function(polls.df){
  polls.df %>% mutate(date = ymd(enddate),
                      week_start = floor_date(date, unit = "week"),
                      # need to add 7 hours b/c for some reason off by 7 hours
                      week_end = as.Date(ceiling_date(as.POSIXct(date + hours(7)), unit = "week", change_on_boundary = TRUE))) %>% 
    group_by(week_end) %>%
    summarise(week_mean_clintonTrumpDiff = mean(clintonTrumpDiff), 
              week_var_clintonTrumpDiff = var(clintonTrumpDiff),
              week_polls_total = n(), week_start = unique(week_start)) %>%
    replace_na(list(week_var_clintonTrumpDiff = 0))
}



# a variety of plotting methods for topic plots and topic-poll plots

# graph topics over time vs. poll performance

library(ggplot2)
library(lubridate)
library(ggpubr)

topicPlotPoint <- function(topicNum, doc.topics.df, xlab=TRUE, trend=FALSE, title = paste("Proportion of Speech Attributable to Topic", topicNum)){
  p <- ggplot(doc.topics.df %>% filter(topic==topicNum), aes(ymd(date))) + 
    geom_point(aes(y = gamma, colour = "Clinton"), data = doc.topics.df %>% filter(topic==topicNum, author=="Hillary Clinton")) + 
    geom_point(aes(y = gamma, colour = "Trump"), data = doc.topics.df %>% filter(topic==topicNum, author=="Donald Trump")) + 
    scale_color_manual(name = "Candidate", values = c(Clinton = "blue", Trump = "red")) + 
    labs(colour = "Candidate", y = "Topic Proportion", x = "Date",
         title = title,
         subtitle = paste("Top words:", doc.topics.df %>% filter(topic==topicNum) %>% dplyr::select(top_words) %>% ungroup() %>% dplyr::select(top_words) %>% unique()))
  if(xlab){
    p <- p + theme(legend.position="none", plot.title = element_text(size=12))
  } else{
    p <- p + theme(legend.position="none", axis.title.x=element_blank(),
                   axis.text.x=element_blank(), plot.title = element_text(size=12))
  }
  if(trend){
    p <- p + geom_smooth(aes(y = gamma, colour = "Clinton"), data = doc.topics.df %>% filter(topic==topicNum, author=="Hillary Clinton")) +
      geom_smooth(aes(y = gamma, colour = "Trump"), data = doc.topics.df %>% filter(topic==topicNum, author=="Donald Trump"))
  }
  p
}


topicPlotSmoothed <- function(topicNum, doc.topics.df, xlab=TRUE, title = paste("Proportion of Speech Attributable to Topic", topicNum)){
  p <- if(xlab){
    ggplot(doc.topics.df %>% filter(topic==topicNum), aes(ymd(date))) + 
      geom_smooth(aes(y = gamma, colour = "Clinton"), data = doc.topics.df %>% filter(topic==topicNum, author=="Hillary Clinton")) + 
      geom_smooth(aes(y = gamma, colour = "Trump"), data = doc.topics.df %>% filter(topic==topicNum, author=="Donald Trump")) + 
      scale_color_manual(name = "Candidate", values = c(Clinton = "blue", Trump = "red")) + 
      labs(colour = "Candidate", y = "Topic Proportion", x = "Date",
           title = title,
           subtitle = paste("Top words:", doc.topics.df %>% filter(topic==topicNum) %>% select(top_words) %>% ungroup() %>% select(top_words) %>% unique()) 
      ) + theme(legend.position="none", plot.title = element_text(size=12))
  } else{
    ggplot(doc.topics.df %>% filter(topic==topicNum), aes(ymd(date))) + 
      geom_smooth(aes(y = gamma, colour = "Clinton"), data = doc.topics.df %>% filter(topic==topicNum, author=="Hillary Clinton")) + 
      geom_smooth(aes(y = gamma, colour = "Trump"), data = doc.topics.df %>% filter(topic==topicNum, author=="Donald Trump")) + 
      scale_color_manual(name = "Candidate", values = c(Clinton = "blue", Trump = "red")) + 
      labs(colour = "Candidate", y = "Topic Proportion", x = "Date",
           title = title,
           subtitle = paste("Top words:", doc.topics.df %>% filter(topic==topicNum) %>% select(top_words) %>% ungroup() %>% select(top_words) %>% unique()) 
      ) + theme(legend.position="none", axis.title.x=element_blank(),
                axis.text.x=element_blank(), plot.title = element_text(size=12))
  }
  print(p$plot)
}



# pollsTrimmed <- polls %>% filter(ymd(enddate) >= min(ymd(doc.topics.tidy$date)))

pollPlot <- function(doc.topics.df){
  pollsTrimmed <- polls %>% filter(ymd(enddate) >= min(ymd(doc.topics.df$date)))
  ggplot(pollsTrimmed, aes(ymd(enddate))) + 
    geom_smooth(aes(y = rawpoll_clinton, colour = "Clinton")) + 
    geom_smooth(aes(y = rawpoll_trump, colour = "Trump")) +
    scale_color_manual(name = "Candidate", values = c(Clinton = "blue", Trump = "red")) + 
    labs(y = "Poll Support", x = "Date") + 
    theme(legend.position="none")
}

# printTopicPollPlots(10, speechTopics.postAug)
# ggarrange(topicPlotSmoothed(10, speechTopics), pollPlot(speechTopics), heights = c(2, 1),
#          ncol = 1, nrow = 2, align = "v")


printTopicPollPlots <- function(ntopics, doc.topics.df){
  for(i in seq_len(ntopics)){
    combPlot <- ggarrange(topicPlotSmoothed(i, doc.topics.df, xlab = FALSE), pollPlot(doc.topics.df), heights = c(2, 1),
                          ncol = 1, nrow = 2, align = "v")
    print(combPlot)
  }
}
# 
# printTopicPollPlots(10, postSep1Corpus)
# ggarrange(topicPlotPoint(2, postSep1Corpus, trend = TRUE, xlab = FALSE), pollPlot(postSep1Corpus), heights = c(2, 1),
#           ncol = 1, nrow = 2, align = "v")
# ggsave("topicPollCombPlot.png")



