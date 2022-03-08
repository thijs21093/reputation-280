# Library
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(vader)
library(data.table)
library(tidyr)
library(zoo)
library(qdapRegex)
library(stringr)
library(lubridate)
library(sjmisc)
library(textutils)

load(file = "./data (not public)/from_tweets/from_tweets_2") # Data
load(file = "./data (not public)/to_tweets/to_tweets_2") # Data

# ======================================================
#           Information
# ======================================================

from.agency <- tibble.from %>%
 arrange(referenced_type != 'replied_to') %>% # remove 'quoted' if 'replied to' is present for same tweet id
  distinct(tweet_id, .keep_all = TRUE) %>%
  mutate(url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # add url
         created_at = as.Date(created_at),
         mutate(text = textutils::HTMLdecode(text))) # Decode html

# Check for duplicates
from.agency %>% 
  group_by(tweet_id) %>%
  filter(n()> 1) %>%
  nrow() # No duplicates

# Check distribution of tweet types  
from.agency %>%
  group_by(referenced_type) %>%
  dplyr::summarise(count = n()) # Count

#           Breakdown
# ======================================================

# Retweets
retweet <- from.agency %>%
  filter(referenced_type == "retweeted" ) # Remove retweets
retweet %>% nrow() # Number initial updates

# Quotes
quote <- from.agency %>%
  filter(referenced_type == "quoted") # Remove quotes
quote %>% nrow() # Number initial updates

# Updates
update <- from.agency %>%
  filter(referenced_type == "no reference" &
          is.na(in_reply_to_user_id)) # Initial updates
update %>% nrow() # Number initial updates

# Tweets to self
agency.to.self <- from.agency %>%
  dplyr::filter(referenced_type != "quoted" & # Remove quotes of own tweets
                in_reply_to_user_id == author_id) # Tweets to self: either start with @[own handle] or a tweet in reply to an agency's own tweet
agency.to.self %>% nrow() # Number of replies to self

# Replies to users
reply.to.user <- from.agency %>%
  filter(referenced_type == "no reference" &
           in_reply_to_user_id != author_id) # Replies to users (but not replies to users' statuses)
reply.to.user %>% nrow() # Number of replies to users

# Reply to status
reply.to.status <- from.agency %>%
  filter(referenced_type == "replied_to" &
        in_reply_to_user_id != author_id)
reply.to.status %>% nrow() # Count of replies to user statuses

#           Information
# ======================================================
information <- bind_rows(update,
                         agency.to.self) # Bind updates and replies to self

information %>% nrow() # Count number of tweets in data

# Information per day
information.day <- information %>%
   group_by(agencyname,
            day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))

# Information per week
information.week <- information %>%
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(week = format(round(as.POSIXct(week), "day")))

# ======================================================
#           Find first activity (joining date)
# ======================================================

joining.date <- from.agency %>%
  group_by(agencyname,
           join.day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(join.day = format(round(as.POSIXct(join.day), "day"))) %>% 
  group_by(agencyname = factor(agencyname)) %>% 
  filter(any(information == 0)) %>% 
  filter(information != 0) %>% 
  arrange(join.day) %>% 
  slice(1)  %>%
  ungroup() %>% 
  complete(agencyname) %>%
  select(-information)

# First activity by any EU agency  
start <- joining.date %>% 
  slice_min(join.day, n = 1) %>%
  select(join.day) %>% 
  as.character()

# ======================================================
#           Responsiveness
# ======================================================

#           Preparing dataframe
# ======================================================

to.agency <- tibble.to %>%
  unnest(cols = attachments, # Unnest attachment
                  keep_empty = TRUE) %>%
  filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
           in_reply_to_user_id != author_id & # Remove tweets to self
           lang == "en") %>% # Remove non-English tweets
  mutate(created_at = as.Date(created_at),
         text = textutils::HTMLdecode(text), # Decode html
         url = paste0("www.twitter.com/", author_id, "/status/", tweet_id))  # Add url

# Check for duplicates
to.agency %>%
  group_by(tweet_id) %>%
  filter(n()> 1) %>%
  nrow() # No duplicates

# Create lists of tweets ids referenced by agencies
response.filter <- from.agency %>%
  filter(!is.na(referenced_id)) %>%
  dplyr::select(referenced_id) %>% 
  as.list()

#           Adding joining date and conversation length
# ======================================================
# Add joining date
to.agency2  <- to.agency %>% 
  full_join(joining.date, by = "agencyname")

# Calculate length of conversations
conversation <- to.agency2 %>% group_by(conversation_id) %>%
  summarise(conversations = length(conversation_id))

# Add length of conversations
agency3 <- to.agency2 %>%
  full_join(conversation, by = "conversation_id")

#           Adding other variables
# ======================================================
response <- to.agency3 %>%

  # Did the agency respond?
  dplyr::mutate(response = case_when(
    tweet_id %in% response.filter[["referenced_id"]] ~ "response", # Code as 'response' when tweet is AT LEAST ONCE responded to
    !tweet_id %in% response.filter[["referenced_id"]] ~ "no.response"), # Code as 'no.response' if not
  
  # Numeric variable
    response.num = case_when(
      response == "response" ~ 1,
      response == "no.response" ~ 0),
  
  # short comment (<= 5 n-grams)
  text.url = str_remove_all(text, "@[[:alnum:]_]{2,}"),# Remove user
  text.url = rm_url(text.url, pattern = pastex("@rm_twitter_url", "@rm_url", "rm_white_lead")),  # Remove url
  ngrams = str_count(text.url, '\\w+'), # Length of text without urls/mentions (@[user])
  short = case_when(
    ngrams <= 5 ~ 1,
    ngrams > 5 ~ 0),
  
  # Question mark in comment
  qm.comment = ifelse(grepl("\\?", text.url), 1, 0), 
  
  # Attachment
  attachement = case_when(media_keys != "NULL" ~ 1, # To do: Check what's included in media_keys
                          TRUE ~ 0),
  
  # Real time
  real.time = difftime(strptime(created_at, format = "%Y-%m-%d"),
                   strptime(start, format = "%Y-%m-%d"),
                  units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric(), # Numeric
  
  # Time on Twitter
  time.on.Twitter = difftime(strptime(created_at, format = "%Y-%m-%d"),
                       strptime(join.day, format = "%Y-%m-%d"),
                       units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric(), # Numeric
  
  # Year
  year =  format(as.POSIXct(created_at), format= "%Y"), 
  
  # weekend versus weekday
         weekday = weekdays(as.Date(created_at)), 
         weekend = case_when(
           weekday == "zaterdag" ~ "weekend", # Saturday
           weekday == "zondag" ~ "weekend",   # Sunday
           TRUE ~ "weekday"))


# ======================================================
#           Constructing variables: from tweets
# ======================================================           Question mark/mention in agency tweet
mentions <- from.agency %>%
  
  # Select variables
  select(c(entities, text, tweet_id, agencyhandle)) %>% 
  
  # Unnest entities and select variables
  unnest(cols = c(entities)) %>%
  select(c(mentions, text, tweet_id, agencyhandle)) %>%
  
  # Unnest mentions and keep empty rows
  unnest(cols = c(mentions)) %>%
  
  # Filter out mentions of self
  mutate(agencyhandle2 = str_remove_all(agencyhandle, "@")) %>%
  filter(agencyhandle2 != username) %>%
  
  # Create list with tweet_ids of tweets that include a mention
  dplyr::select(tweet_id) %>% 
  as.list()

#           Mention/question mark
# ======================================================
engaging <- from.agency %>%
  mutate(
    
    # Does contain a mention?
    mention = case_when(
      tweet_id %in% mentions[["tweet_id"]] ~ 1, # Code as 1 if tweet mentions AT LEAST one user
      !tweet_id %in% mentions[["tweet_id"]] ~ 0), # Code as 0 if not
    
    # Remove handles and urls
    text.url = str_remove_all(text, "@[[:alnum:]_]{2,}"),# Remove user
    text.url = rm_url(text.url, pattern = pastex("@rm_twitter_url", "@rm_url", "rm_white_lead")),  # Remove url  
    
  # Question mark in cleaned agency tweet
  qm.agency = ifelse(grepl("\\?", text.url), 1, 0)) %>%
  select(c(mention, qm.agency, tweet_id))

#           Add new variables to response df
# ======================================================
response2 <- response %>%
  left_join(engaging, # Left join, b/c we only want tweets TO agencies
            by = c("referenced_id" = "tweet_id")) %>% # Link between tweets from agencies and tweet to agencies
  full_join(joining.date, by = "agencyname") # Also add joining date
  
response2 %>%
  select(c(mention, qm.agency)) %>% 
  descr # 0.88% missing

# ======================================================
#           Description of response variable
# ======================================================

#           Check NAs     
# ======================================================

# All responses
response2 %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Non-NAs
response2 %>%
  filter(!is.na(qm.agency)) %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# NAs
response2 %>%
  filter(is.na(qm.agency)) %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Note: higher response in df with NAs because agencies plausibly
# are more likely to respond to replies pertaining to broken links, errors, typos, etc.

# Note: Missingness is caused by tweets that no longer exist (i.e., deleted) or 
# that have not been returned by the API. 

#           Table with responsiveness per agency-halfyear     
# ======================================================
response.agency <- response2 %>% 
  group_by(response, agencyname) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  group_by(agencyname) %>%            # Group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  mutate(ratio = response/(response + no.response))

#           Plot
# ======================================================
response.plot <- response2 %>% 
  group_by(response, agencyname, created_at) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  mutate(halfyear = lubridate::floor_date(created_at, unit = "halfyear")) %>%
  group_by(halfyear, agencyname) %>%            # Group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  group_by(agencyname) %>% 
  mutate(max = case_when(
    max(no.response) > max(response) ~ max(no.response),
    max(response) > max(no.response) ~ max(response))) %>% 
  ungroup() %>%
  arrange(max) %>%               # sort dataframe
  mutate(agencyname = factor(agencyname, unique(agencyname))) # reset factor-column based on that order

# Creating a plot 
response.plot %>%
  ggplot(aes(x = as.Date(halfyear))) +  
  facet_wrap(facets = ~agencyname,  nrow = 9, scales = "free_y") +
  geom_point(aes(y = (response + 1), color = "Response", shape = "Response"), size = 2) +
  geom_point(aes(y = (no.response + 1), color = "No response", shape = "No response"), size = 2) +
  geom_smooth(aes(y = (no.response + 1), color = "No response"), se = FALSE) +
  geom_smooth(aes(y = (response + 1), color = "Response"), se = FALSE) +
  scale_colour_manual("", values = c("#1aafd4", "#D43F1A")) +
  scale_shape_manual("", values = c(17, 16)) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%y") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black", size = 2),
        legend.key.size = unit(2, "line")) +
  labs(color  = "Guide name", shape = "Guide name") +
  xlab("Year") + ylab("Count (log)") + labs(title = "Responsiveness on Twitter per 6 months")

#           Consequences
# ======================================================
sentiment <- vader_df(response2$text,
                            incl_nt = T,
                            neu_set = T,
                            rm_qm = T) # Calculate valence of tweets

tweet.data <- merge(sentiment,
                      response,
                      by.y = "text",
                      by.x = "text") %>% # Merge scores with data
  distinct() %>% # TO DO: Why are duplicates created?
  mutate(sentiment = case_when(
    compound <= -0.05 ~ "negative",
    compound > -0.05 & compound < 0.05 ~ "neutral",
    compound >= 0.05 ~ "positive"))

# Distribution
tweet.data %>%
  group_by(sentiment) %>%
  dplyr::summarise(count = n())

# Consequences per day
consequences.day <- tweet.data %>%
  dplyr::group_by(agencyname, day = cut(created_at, "day"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))

# Consequences per week
consequences.week <- tweet.data %>%
  dplyr::group_by(agencyname, week = cut(created_at, "week"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(week = format(round(as.POSIXct(week), "day")))

# ======================================================
#           Joining data with lags
# ======================================================

# Day

# Join dataframes
twitter.day <- information.day %>%
  full_join(consequences.day,  by = c("agencyname", "day")) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))

# Load media data
media <- read.csv("media/media_day.csv") %>%
  mutate(day = format(round(as.POSIXct(day), "day"))) %>%
  select(-X)

# ======================================================
#           Joining dataframes: tweet-level
# ======================================================

#           Tweet-level
# ======================================================
data.tweet <- tweet.data %>%
  select(tweet_id, conversation_id, created_at, agencyname,
         response, response.num, short, qm.comment, attachement,
         real.time, time.on.Twitter, year, mention, qm.agency, compound, conversations)

# ======================================================
#           Joining dataframes: agency-week panel
# ======================================================
# Week
data.week <- tweet.data %>%
  dplyr::group_by(agencyname, week = cut(created_at, "week"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(week = format(round(as.POSIXct(week), "day")))

# ======================================================
#           Rolling sum/lags
# ======================================================
data.day <- all.vars %>%
  arrange(day) %>% # Arrange by day
  group_by(agencyname) %>%
dplyr::mutate(
              # Valence on Twitter in past 30 days
              criticism.30d = rollapplyr(negative, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              praise.30d = rollapplyr(positive, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              neutral.30d = rollapplyr(neutral, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              valence.30d = ifelse(!(praise.30d + neutral.30d + criticism.30d), 0, (praise.30d - criticism.30d) / (praise.30d + neutral.30d + criticism.30d)),
              
              # Valence on Twitter in past 90 days
              criticism.90d = rollapplyr(negative, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              praise.90d = rollapplyr(positive, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              neutral.90d = rollapplyr(neutral, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              valence.90d = ifelse(!(praise.90d + neutral.90d + criticism.90d), 0, (praise.90d - criticism.90d) / (praise.90d + neutral.90d + criticism.90d)),
              
              # Valence on Twitter in past 365 days
              criticism.365d = rollapplyr(negative, list(seq(-365, -1)), sum, fill = NA, align = "right"),
              praise.365d = rollapplyr(positive, list(seq(-365, -1)), sum, fill = NA, align = "right"),
              neutral.365d = rollapplyr(neutral, list(seq(-365, -1)), sum, fill = NA, align = "right"),
              valence.365d = ifelse(!(praise.365d + neutral.365d + criticism.365d), 0, (praise.365d - criticism.365d) / (praise.365d + neutral.365d + criticism.365d)),
              
              # Media count
              media.count.30d = rollapplyr(#media, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              media.count.90d = rollapplyr(#media, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              media.count.365d = rollapplyr(#media, list(seq(-365, -1)), sum, fill = NA, align = "right"),
              
              # Media valence
              media.valence.30d = rollapplyr(#media, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              media.valence.90d = rollapplyr(#media, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              media.valence.365d = rollapplyr(#media, list(seq(-365, -1)), sum, fill = NA, align = "right"),
              
              # Information
              information.1w = rollapplyr(information, list(seq(-7, -1)), sum, fill = NA, align = "right"),
              information.30d = rollapplyr(information, list(seq(-30, -1)), sum, fill = NA, align = "right"),
              information.90d = rollapplyr(information, list(seq(-90, -1)), sum, fill = NA, align = "right"),
              information.365d = rollapplyr(information, list(seq(-365, -1)), sum, fill = NA, align = "right"))

# ======================================================
#           Merging
# ======================================================

# Set time variables to for dates when agency was not yet on Twitter
total.day <- twitter.day %>%
  mutate_at(.vars = c("information",
                      "positive",
                      "neutral",
                      "negative",
                      "media_count",
                      "media_valence"),
            funs(ifelse(joined == "no", NA, .))) %>% # Twitter variable to NA when agency was not on Twitter
  mutate(day = format(round(as.POSIXct(day), "day")))

tweet.data <- tweet.data %>%
  mutate(day = format(round(as.POSIXct(created_at), "day")), .keep = "unused")

data.merge <- tweet.data %>%
  left_join(data.day,
            by = c("agencyname", "day"))

save(data.merge, file = "data_day.Rda")



