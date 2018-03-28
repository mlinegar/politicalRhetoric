# this can take a while to run, at least on my small (4gb RAM) laptop
# as such, I'd recommend either:
# reducing the number of iterations by setting the burnIn and numRuns options in the makeCorpus function
# or just read in the generated output files, which are already uploaded
library(tidyverse)

processedSpeeches <- read_csv("processedSpeeches.csv")
collectedSpeeches.metadata <- read_csv("collectedSpeeches.metadata.csv")
# now want to slice corpus into 4 sections
overallCorpus <- makeCorpus(processedSpeeches, ntopics, 
                            metaData = collectedSpeeches.metadata,
                            metaDataJoinByCorpusCol = "document",
                            metaDataJoinByMetaDataCol = "author_speech_id")
write.csv(overallCorpus, "overallCorpus.csv")

postSep1Corpus <- makeCorpus(processedSpeeches, ntopics, dateFilter = "ymd(date) > mdy(September 1, 2016)",
                                    metaData = modernPoliticalAddresses.metadata,
                                    metaDataJoinByCorpusCol = "document",
                                    metaDataJoinByMetaDataCol = "author_speech_id")
write.csv(postSep1Corpus, "postSep1Corpus.csv")
