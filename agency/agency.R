# Library
library(dplyr)
library(tidyr)
library(academictwitteR)

# Data
load(file="./Data/all_ageny_tweets") # Data

# ======================================================
#           Pre-processing
# ======================================================

# Moritz: I went for the loop function, which is super not elegant, but it works. Below is a testrun.
test$referenced_type = "no reference"
test$referenced_id = 0

for(i in 1:nrow(test)){
  test$referenced_type[i] = ifelse(is.null(test$referenced_tweets[i][[1]])==F, test$referenced_tweets[i][[1]]$type, 0)
  test$referenced_id[i] = ifelse(is.null(test$referenced_tweets[i][[1]])==F, test$referenced_tweets[i][[1]]$id, 0)
}

# now for the whole list of tweets
tweets = list()
for(f in 1:length(alltweets)){
  temporary = alltweets[[f]]
  temporary$referenced_type = "no reference"
  temporary$referenced_id = 0
  for(i in 1:nrow(test)){
    temporary$referenced_type[i] = ifelse(is.null(temporary$referenced_tweets[i][[1]])==F, temporary$referenced_tweets[i][[1]]$type, 0)
    temporary$referenced_id[i] = ifelse(is.null(temporary$referenced_tweets[i][[1]])==F, temporary$referenced_tweets[i][[1]]$id, 0)
    tweets[[f]] = temporary
  }
}

save(tweets, file="tweets")
