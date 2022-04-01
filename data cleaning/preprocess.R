# Library
library(dplyr)
library(tidyr)
library(lubridate)

<<<<<<< HEAD
# Set time zone
Sys.setenv(TZ = 'GMT')
=======
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# ======================================================
#           From agencies
# ======================================================

# Clear environemnt
rm(list = ls())

# Data
<<<<<<< HEAD
load(file="./Data/data_tweets_4/alltweets4") # Data
=======
load(file="./Data/alltweets3") # Data
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Create dataframe
for (i in seq(alltweets)){
  assign(paste0("df", i), alltweets[[i]])} # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- bind_rows(mget(names(dfs)[dfs])) # Bind dataframes

# Thijs: added a line which changes time zone to GMT
# Manipulation MORITZ VERSION
tibble.from <-  df %>%
  as_tibble() %>% # As tibble
  rename(tweet_id = id) %>% # rename to avoid conflicting names
  unnest(cols = referenced_tweets, # Note: Some tweets are both quotes and replies to statuses.
         keep_empty = TRUE) %>%    # For these tweets, a second row is created when unnesting referenced_tweets.
<<<<<<< HEAD
  distinct(tweet_id, .keep_all = TRUE) %>% # TO DO: why are duplicates created?
  rename(referenced_id = id,
  referenced_type = type) %>% # Unnesting referenced tweets
  mutate(referenced_type = replace_na(referenced_type, "no reference"),
         created_at = with_tz(created_at, "GMT"))
=======
  rename(referenced_id = id,
  referenced_type = type) %>% # Unnesting referenced tweets
  mutate(referenced_type = replace_na(referenced_type, "no reference"))
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

tibble.from %>%
  group_by(referenced_type) %>%
  dplyr::summarise(count = n()) # Count

<<<<<<< HEAD
save(tibble.from, file = "from_tweets_3")
=======
save(tibble.from, file = "from_tweets_2")
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# ======================================================
#           Replies to agencies
# ======================================================

# Clear environment
rm(list = ls())

# Data
<<<<<<< HEAD
load(file = "./Data/data_replies_5/allreplies_5") # Data, set path
=======
load(file = "./Data/all_replies_3") # Data, set path
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Create dataframe
for (i in seq(allreplies)){
  assign(paste0("df.to", i), allreplies[[i]])} # Create seperate dataframes

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
<<<<<<< HEAD
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference"),
         created_at = with_tz(created_at, "GMT"))

save(tibble.to, file = "to_tweets_3")

=======
  mutate(referenced_type = tidyr::replace_na(referenced_type, "no reference"))

save(tibble.to, file = "to_tweets_2")
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
