# Library
library(dplyr)
library(tidyr)

# ======================================================
#           From agencies
# ======================================================

# Clear environemnt
rm(list = ls())

# Data
load(file="./Data/all_ageny_tweets") # Data

# Create dataframe
for (i in seq(alltweets))
  assign(paste0("df", i), alltweets[[i]]) # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- bind_rows(mget(names(dfs)[dfs])) # Bind dataframes

# Manipulation
tibble.from <-  df %>% 
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Unnest column
         keep_empty = TRUE) %>% # Remove quoted tweets
  distinct(tweet_id, .keep_all = TRUE) %>% # TO DO: why are duplicates created?
  rename(referenced_id = id,
         referenced_type = type) %>% # Unnesting referenced tweets
  mutate(referenced_type = replace_na(referenced_type, "no reference"))

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
load(file="./Data/all_replies") # Data

# Create dataframe
for (i in seq(allreplies))
  assign(paste0("df.to", i), allreplies[[i]]) # Create seperate dataframes

dfs.to <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df.to <- bind_rows(mget(names(dfs.to)[dfs.to])) # Bind dataframes

# Manipulation
tibble.to <-  df.to %>% 
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Unnest column
         keep_empty = TRUE) %>% # Remove quoted tweets
  distinct(tweet_id, .keep_all = TRUE) %>% # TO DO: why are duplicates created?
  rename(referenced_id = id,
         referenced_type = type) %>% # Unnesting referenced tweets
  mutate(referenced_type = replace_na(referenced_type, "no reference"))

save(tibble.to, file = "to_tweets")

# Moritz: I went for the loop function, which is super not elegant, but it works. Below is a testrun.
# 
# test$referenced_type = "no reference"
# test$referenced_id = 0

# for(i in 1:nrow(test)){
#   test$referenced_type[i] = ifelse(is.null(test$referenced_tweets[i][[1]])==F, test$referenced_tweets[i][[1]]$type, 0)
#   test$referenced_id[i] = ifelse(is.null(test$referenced_tweets[i][[1]])==F, test$referenced_tweets[i][[1]]$id, 0)
# }

# now for the whole list of tweets
# tweets = list()
# for(f in 1:length(alltweets)){
#   temporary = alltweets[[f]]
#   temporary$referenced_type = "no reference"
#   temporary$referenced_id = 0
#   for(i in 1:nrow(test)){
#     temporary$referenced_type[i] = ifelse(is.null(temporary$referenced_tweets[i][[1]])==F, temporary$referenced_tweets[i][[1]]$type, 0)
#     temporary$referenced_id[i] = ifelse(is.null(temporary$referenced_tweets[i][[1]])==F, temporary$referenced_tweets[i][[1]]$id, 0)
#     tweets[[f]] = temporary
#   }
# }

save(tweets, file="tweets")