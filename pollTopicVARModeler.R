# applies a VAR model to poll and topic-speech data
# note that this is a discrete-time VAR model, NOT a continuous-time VAR
# this is because the MONOCAR model as implemented in the CARMAgeddon package fails to converge on my data
# this appears to be the bug in the code; I've contacted the package author

# use Kalman filter to fill in missing observations!
# this way we can use a traditional VAR model
# there will be some sacrifices
# we can approximate the continuous time nature of the MONOCAR model for polls
# the MONOCAR model does this by treating the polls as being the average of the poll 
# from the start-date to the end-date
# I *think* we can approximate this by "spreading" the polls over a discrete space instead
# and the Kalman filter will handle the topics, or else we'll set missing speech-days to zero

# first the polls
library(tidyverse)
library(lubridate)
polls <- read_csv("http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv")
polls <- polls %>% mutate(startdate = mdy(startdate), 
                          enddate = mdy(enddate)) %>%
  filter(state == "U.S.", type == "polls-only", startdate > ymd("2016-07-1"),
         enddate < ymd("2016-11-9")) %>% 
  mutate(clintonTrumpDiff = rawpoll_clinton - rawpoll_trump)

# we want to spread each poll over its sample, so that we have one obs per day a poll ran
# taken from here:
# https://stackoverflow.com/questions/20429561/reshape-data-frame-to-add-rows-for-unique-year-and-add-new-sequence
## Figure out how much longer we need to make the data
Expand <- as.numeric(polls$enddate - polls$startdate) + 1
polls2 <- polls[rep(rownames(polls), Expand), ]
# Generate the sequence of years, 
##    again using the "Expand" vector just created
polls2$day <- unlist(lapply(seq_along(Expand), function(x) 
  polls$startdate[x] + (sequence(Expand[x])-1)), use.names = FALSE)
polls2$day <- as.Date(polls2$day, origin = "1970-01-01")
polls2 <- polls2 %>% mutate(pollLength = as.numeric(enddate - startdate) + 1, 
                            pollDaySample = samplesize / pollLength)

dailyPollAvg <- polls2 %>% group_by(day) %>%
  summarise(dailySampleSize = sum(pollDaySample), 
            dailyAvg = mean(clintonTrumpDiff),
            dailyWeightedAvg = weighted.mean(clintonTrumpDiff, pollDaySample),
            npolls = n())
# here I difference the data - otherwise the roots on the VAR model are sometimes non-stationary
dailyPollAvg$dailyAvg_diffed <- c(0, diff(dailyPollAvg$dailyAvg))
dailyPollAvg$dailyWeightedAvg_diffed <- c(0, diff(dailyPollAvg$dailyWeightedAvg))

# how does the average difference compare to the weighted difference?
ggplot(dailyPollAvg, aes(x = day)) +
  geom_smooth(aes(y = dailyAvg, colour = "red")) + 
  geom_smooth(aes(y = dailyWeightedAvg, colour = "blue")) 

# in the next part I show I want to restrict the sample to post September 1, do that for polls here
dailyPollAvg.postSep1 <- dailyPollAvg %>% filter(day >= mdy("September 1, 2016"))

# I think that's all that needs to be done for polls
# now for speeches - smooth with Kalman filter
# first need to coerce each day into an observation
postSep1Corpus <- read_csv("postSep1Corpus.csv")
electionDates.postSep1 <- seq.Date(as.Date("2016-09-01"), as.Date("2016-11-08"), by = "day")
electionDates.postSep1 <- data.frame(date = electionDates.postSep1)
dailySpeeches.postSep1 <- postSep1Corpus %>% group_by(author, topic, date) %>% summarise(gamma = mean(gamma))
# long to wide
dailySpeeches.postSep1 <- dailySpeeches.postSep1 %>% spread(topic, gamma)

dailySpeeches.postSep1.trump <- dailySpeeches.postSep1 %>% filter(author=="Donald Trump")
dailySpeeches.postSep1.trump.fullSample <- left_join(electionDates.postSep1, dailySpeeches.postSep1.trump)
dailySpeeches.postSep1.clinton <- dailySpeeches.postSep1 %>% filter(author=="Hillary Clinton")
dailySpeeches.postSep1.clinton.fullSample <- left_join(electionDates.postSep1, dailySpeeches.postSep1.clinton)

# first setting missing speech-days to zero
dailySpeeches.postSep1.clinton.zero <- dailySpeeches.postSep1.clinton.fullSample
dailySpeeches.postSep1.clinton.zero$author <- "Hillary Clinton"
dailySpeeches.postSep1.clinton.zero[is.na(dailySpeeches.postSep1.clinton.zero)] <- 0
dailySpeeches.postSep1.trump.zero <- dailySpeeches.postSep1.trump.fullSample
dailySpeeches.postSep1.trump.zero$author <- "Donald Trump"
dailySpeeches.postSep1.trump.zero[is.na(dailySpeeches.postSep1.trump.zero)] <- 0
zeroedSpeeches <- rbind(dailySpeeches.postSep1.clinton.zero, dailySpeeches.postSep1.trump.zero)

# now using Kalman filter to smooth
library(imputeTS)
dailySpeeches.postSep1.clinton.imputed <- data.frame(author = dailySpeeches.postSep1.clinton.fullSample$author, date = dailySpeeches.postSep1.clinton.fullSample$date)
dailySpeeches.postSep1.clinton.imputed <- dailySpeeches.postSep1.clinton.imputed %>% mutate(imputed = ifelse(is.na(author), 1, 0), author = "Hillary Clinton")
dailySpeeches.postSep1.trump.imputed <- data.frame(author = dailySpeeches.postSep1.trump.fullSample$author, date = dailySpeeches.postSep1.trump.fullSample$date)
dailySpeeches.postSep1.trump.imputed <- dailySpeeches.postSep1.trump.imputed %>% mutate(imputed = ifelse(is.na(author), 1, 0), author = "Donald Trump")

# this for loop actually does the imputation for each topic-author combination
ntopics <- 10
for (topic in seq_len(ntopics)) {
  clintonTopic.impute <- na.kalman(dailySpeeches.postSep1.clinton.fullSample %>% dplyr::select(as.character(topic)), model ="StructTS", smooth = TRUE, type = "level")
  dailySpeeches.postSep1.clinton.imputed[, as.character(topic)] <- clintonTopic.impute
  trumpTopic.impute <- na.kalman(dailySpeeches.postSep1.trump.fullSample %>% dplyr::select(as.character(topic)), model ="StructTS", smooth = TRUE, type = "level")
  dailySpeeches.postSep1.trump.imputed[, as.character(topic)] <- trumpTopic.impute
}

stopifnot(nrow(dailyPollAvg.postSep1)==nrow(dailySpeeches.postSep1.clinton.imputed),
          nrow(dailyPollAvg.postSep1)==nrow(dailySpeeches.postSep1.trump.imputed))



# now can try doing a VAR model!
library(vars)
clintonTopicPoll.postSep1 <- left_join(dailySpeeches.postSep1.clinton.imputed, dailyPollAvg.postSep1, by = c("date" = "day"))
trumpTopicPoll.postSep1 <- left_join(dailySpeeches.postSep1.trump.imputed, dailyPollAvg.postSep1, by = c("date" = "day"))
clinton.zeroedSpeeches.postSep1 <- zeroedSpeeches.postSep1 %>% filter(author=="Hillary Clinton") %>% left_join(dailyPollAvg.postSep1, by = c("date" = "day"))
trump.zeroedSpeeches.postSep1 <- zeroedSpeeches.postSep1 %>% filter(author=="Donald Trump") %>% left_join(dailyPollAvg.postSep1, by = c("date" = "day"))
# what's the ideal lag length P?
# first for zeroed
idealP.clinton <- vector()
idealP.trump <- vector()
for (topic in seq_len(ntopics)) {
  p.clinton <- VARselect(clinton.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), lag.max = 14, type = "const")
  p.trump <- VARselect(trump.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), lag.max = 14, type = "const")
  idealP.clinton <- c(idealP.clinton, p.clinton$selection[1])
  idealP.trump <- c(idealP.trump, p.trump$selection[1])
}
print("Ideal Clinton lags:")
print(idealP.clinton)
print("Ideal Trump lags:")
print(idealP.trump)
mean(as.numeric(idealP.clinton))
mean(as.numeric(idealP.trump))
# relatively wide range, especially for Trump (one is 14!)
# what to do?
# mean best lag of 1.3 for clinton, 2.6 for trump

# now for imputed
idealP.clinton <- vector()
idealP.trump <- vector()
for (topic in seq_len(ntopics)) {
  p.clinton <- VARselect(clintonTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), lag.max = 14, type = "const")
  p.trump <- VARselect(trumpTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), lag.max = 14, type = "const")
  idealP.clinton <- c(idealP.clinton, p.clinton$selection[1])
  idealP.trump <- c(idealP.trump, p.trump$selection[1])
}
print("Ideal Clinton lags:")
print(idealP.clinton)
print("Ideal Trump lags:")
print(idealP.trump)
mean(as.numeric(idealP.clinton))
mean(as.numeric(idealP.trump))
# smaller range - all 1-4 lags is best
# means are 1.7 for clinton, 1.5 for trump

# this next part runs the VAR model for lag with p=2 for bivariate VAR models
# this is done for both Clinton and Trump speeches, for imputed and zeroed speeches

# zeroed
for(topic in seq_len(ntopics)){
  clinton2var.zeroed.postSep1.topici <- VAR(clinton.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  print(summary(clinton2var.zeroed.postSep1.topici))
}
for(topic in seq_len(ntopics)){
  trump2var.zeroed.postSep1.topici <- VAR(trump.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  print(summary(trump2var.zeroed.postSep1.topici))
}

# imputed
for(topic in seq_len(ntopics)){
  clinton2var.postSep1.topici <- VAR(clintonTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  print(summary(clinton2var.postSep1.topici))
}

for(topic in seq_len(ntopics)){
  trump2var.postSep1.topici <- VAR(trumpTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  print(summary(trump2var.postSep1.topici))
}

# almost none of these models have a significant relationship between polls and topics
# one question is what the average effect of polls-on-polls is (lag 1)

# average coefficient on dailyAvg_diffed.l1
# this is fast enough to just redo without much effort
dailyAvg_diffed.l1.coefs <- vector()
for(topic in seq_len(ntopics)){
  clinton2var.zeroed.postSep1.topici <- VAR(clinton.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  trump2var.zeroed.postSep1.topici <- VAR(trump.zeroedSpeeches.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  clinton2var.postSep1.topici <- VAR(clintonTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  trump2var.postSep1.topici <- VAR(trumpTopicPoll.postSep1 %>% dplyr::select(dailyAvg_diffed, as.character(topic)), p = 2, type = "const")
  dailyAvg_diffed.l1.coefs <- c(dailyAvg_diffed.l1.coefs,
                                as.numeric(clinton2var.zeroed.postSep1.topici$varresult$dailyAvg_diffed$coefficients[1]),
                                as.numeric(trump2var.zeroed.postSep1.topici$varresult$dailyAvg_diffed$coefficients[1]),
                                as.numeric(clinton2var.postSep1.topici$varresult$dailyAvg_diffed$coefficients[1]),
                                as.numeric(trump2var.postSep1.topici$varresult$dailyAvg_diffed$coefficients[1]))
}
mean(dailyAvg_diffed.l1.coefs)

# we might want to run bigger than just bivariate models
# this next part does that

# now for bigger models
# there are 3 types: just clinton, just trump, both
# also want to have different sizes of models: 
# say, above a certain average useage threshold

# first wide to long to make tidy
imputedSpeeches.postSep1 <- rbind(dailySpeeches.postSep1.clinton.imputed, dailySpeeches.postSep1.trump.imputed)

imputedSpeeches.postSep1.long <- imputedSpeeches.postSep1 %>% gather(topic, gamma, `1`:`10`)
zeroedSpeeches.postSep1.long <- zeroedSpeeches.postSep1 %>% gather(topic, gamma, `1`:`10`)
library(stringr)
# now long to wide to get into right format for VAR
imputedCombinedCorpus.postSep1 <- imputedSpeeches.postSep1.long %>% mutate(candidateTopic = paste0(word(author, -1), topic)) %>%
  dplyr::select(candidateTopic, date, gamma) %>%
  spread(candidateTopic, gamma) 
imputedCombinedCorpus.postSep1 <- imputedCombinedCorpus.postSep1 %>% left_join(dailyPollAvg.postSep1, by = c("date" = "day"))

zeroedCombinedCorpus.postSep1 <- zeroedSpeeches.postSep1.long %>% mutate(candidateTopic = paste0(word(author, -1), topic)) %>%
  dplyr::select(candidateTopic, date, gamma) %>%
  spread(candidateTopic, gamma) 
zeroedCombinedCorpus.postSep1 <- zeroedCombinedCorpus.postSep1 %>% left_join(dailyPollAvg.postSep1, by = c("date" = "day"))

# a few examples:
# now can do bigger VAR!
VARselect(zeroedCombinedCorpus.postSep1 %>% dplyr::select(dailyAvg_diffed, Clinton1, Clinton2, Clinton3, Clinton8), lag.max = 7, type = "const")
# 2 appears to be the consensus
clintonVar.postSep1.1238 <- VAR(zeroedCombinedCorpus.postSep1 %>% dplyr::select(dailyAvg_diffed, Clinton1, Clinton2, Clinton3, Clinton8), p = 2, type = "const")
summary(clintonVar.postSep1.1238)

VARselect(zeroedCombinedCorpus.postSep1 %>% dplyr::select(dailyAvg_diffed, Trump5, Trump7, Trump9, Trump10), lag.max = 7, type = "const")
# 2-5 all about equally high for AIC, let's do 2 again
trumpVar.postSep1.57910 <- VAR(zeroedCombinedCorpus.postSep1 %>% dplyr::select(dailyAvg_diffed, Trump5, Trump7, Trump9, Trump10), p = 2, type = "const")
summary(trumpVar.postSep1.57910)
