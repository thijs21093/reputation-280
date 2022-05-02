# Library
library(dplyr)
library(tidyr)
library(lubridate)

# ======================================================
#           From agencies
# ======================================================

# Clear environemnt & check timezone
rm(list = ls())
Sys.timezone()
Sys.time()

# Data
load(file="./data (not public)/from_tweets/alltweets4") # Data 

# Create dataframe
for (i in seq(alltweets)){
  assign(paste0("df", i), alltweets[[i]])} # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- bind_rows(mget(names(dfs)[dfs])) # Bind dataframes

tibble.from.time <-  df %>%
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Note: Some tweets are both quotes and replies to statuses.
         keep_empty = TRUE) %>%    # For these tweets, a second row is created when unnesting referenced_tweets.
  distinct(tweet_id, .keep_all = TRUE) %>%
  rename(referenced_id = id,
  referenced_type = type) %>% # Unnesting referenced tweets
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference")) %>%
  dplyr::select(tweet_id, created_at)

# Tweets
load(file = "./data (not public)/from_tweets/from_tweets_3")

tibble.from <- tibble.from %>%
  select(-created_at) %>%
  full_join(tibble.from.time, by = "tweet_id")

tibble.from %>%
  group_by(referenced_type) %>% 
  dplyr::summarise(count = n()) # Count

save(tibble.from, file = "from_tweets_3a.Rdata")

# ======================================================
#           Replies to agencies
# ======================================================

# Clear environment
rm(list = ls())

# Data
load(file = "./data (not public)/to_tweets/allreplies_5") # Data, set path

# Create dataframe
for (i in seq(allreplies)){
  assign(paste0("df.to", i), allreplies[[i]])} # Create seperate dataframes

dfs.to <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df.to <- bind_rows(mget(names(dfs.to)[dfs.to])) # Bind dataframes

# Manipulation
tibble.to.time <-  df.to %>% 
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Note: Some tweets are both quotes and replies to statuses.
         keep_empty = TRUE) %>%    # For these tweets, a second row is created when unnesting referenced_tweets.
  distinct(tweet_id, .keep_all = TRUE) %>%
  rename(referenced_id = id,     
         referenced_type = type) %>% 
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference")) %>%
  dplyr::select(tweet_id, created_at)
  

load(file = "./data (not public)/to_tweets/tweetsTO_final.RData")

tibble.to <- tweetsTO_final %>%
  select(-created_at) %>%
  left_join(tibble.to.time, by = "tweet_id")

save(tibble.to, file = "allreplies_5a.Rdata")


