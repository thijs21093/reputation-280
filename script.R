# Library
library(dplyr)
library(tidyr)
library(academictwitteR)

# Data
load(file="./Data/all_ageny_tweets") # Data
load(file="./Data/audiencecounts") # Data

# ======================================================
#           Pre-processing
# ======================================================
tibble <- bind_tweets(data_path = "to_agencies") %>% 
  as_tibble %>% # As tibble
  rename(tweet_id = id) # Code for creating tibble from JSON, see: https://cran.r-project.org/web/packages/academictwitteR/vignettes/academictwitteR-tidy.html

tibble_unlist <- unnest(tibble,
            cols = referenced_tweets, # Unnest column
            keep_empty = TRUE) %>%
  rename(referenced_id = id,
         referenced_type = type) # Unnesting referenced tweets


