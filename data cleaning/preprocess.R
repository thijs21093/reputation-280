# Library
library(dplyr)
library(tidyr)
library(textutils)
# ======================================================
#           From agencies
# ======================================================

# Clear environemnt
rm(list = ls())

# Data
load(file="./data (not public)/from_tweets/alltweets2") # Data

# Create dataframe
for (i in seq(alltweets)){
  assign(paste0("df", i), alltweets[[i]])} # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- bind_rows(mget(names(dfs)[dfs])) # Bind dataframes

# Manipulation
tibble.from <-  df %>% 
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Note: Some tweets are both quotes and replies to statuses.
         keep_empty = TRUE) %>%    # For these tweets, a second row is created when unnesting referenced_tweets.
    rename(referenced_id = id,     # The duplicate will later be deleted when filtering out quotes.
         referenced_type = type) %>% 
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference"),
         text = textutils::HTMLdecode(text)) %>% # Decode html
 unnest(cols = attachments, # Unnest attachment
      keep_empty = TRUE)

tibble.from %>%
  group_by(referenced_type) %>%
  dplyr::summarise(count = n()) # Count

save(tibble.from, file = "from_tweets")

# ======================================================
#           Replies to agencies
# ======================================================

# Clear environment
rm(list = ls())

# Data
load(file = "./data (not public)/to_tweets/to_tweets") # Data, set path

# Create dataframe
for (i in seq(allreplies))
  assign(paste0("df.to", i), allreplies[[i]]) # Create seperate dataframes

dfs.to <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df.to <- bind_rows(mget(names(dfs.to)[dfs.to])) # Bind dataframes

# Manipulation
tibble.to <-  df.to %>% 
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Note: Some tweets are both quotes and replies to statuses.
         keep_empty = TRUE) %>%    # For these tweets, a second row is created when unnesting referenced_tweets.
  rename(referenced_id = id,     # The duplicate will later be deleted when filtering out quotes.
         referenced_type = type) %>% 
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference"))

save(tibble.to, file = "to_tweets")