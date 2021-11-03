# Library
library(dplyr)

# Data
load(file="./Data/tweets") # Data

# ======================================================
#           Pre-processing
# ======================================================
for (i in seq(tweets))
  assign(paste0("df", i), tweets[[i]]) # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- bind_rows(mget(names(dfs)[dfs])) # Bind dataframes

from.agency <- df %>%
  mutate(created_at = as.Date(created_at)) %>%
  filter(referenced_type != "retweeted" & # Remove retweets
         referenced_type != "quoted") %>% # Remove quoted tweets
  distinct(id, .keep_all = TRUE) # remove duplicates

# Information

# TO DO: what does '0' mean in referenced_type

update <- from.agency %>%
  filter(referenced_type == "no reference") # Initial updates + replies to users (not status)

agency.to.self <- from.agency %>%
  dplyr::filter(in_reply_to_user_id == author_id) # Tweets to self
agency.to.self %>% nrow() # Number of replies to self

agency.reply.to.user <- from.agency %>%
  filter(referenced_type == "no reference" &
           in_reply_to_user_id != author_id &
           referenced_id == 0) # Replies to users (but not replies to users' statuses)
agency.reply.to.user %>% nrow() # Count of replies to users

information <- bind_rows(agency.to.self,
                         update,
                         agency.reply.to.user) # Bind updates and replies to self
information %>% nrow() # Count number of agency updates


