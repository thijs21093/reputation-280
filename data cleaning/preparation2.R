# Library
library(dplyr)
library(scales)
library(data.table)
library(tidyr)
library(qdapRegex)
library(stringr)
library(lubridate)
library(sjmisc)
library(textutils)
library(stringi)
library(tm)
library(ggplot2)

# Set time zone
Sys.timezone() # Europe/Berlin
Sys.time()

# Tweets
load(file = "./data (not public)/from_tweets/from_tweets_3a.RData")
load(file = "./data (not public)/to_tweets/allreplies_5a.RData")

# Media count
load("./data (not public)/media - outputs/media_week2_new.Rdata")

media.week2 <- media.week2 %>%
  dplyr::rename(agencyname = acronym) 

# ======================================================
#           Information
# ======================================================
from.agency <- tibble.from %>%
  unnest(cols = public_metrics, # Unnest public metrics
         keep_empty = TRUE)%>%
  mutate(text = textutils::HTMLdecode(text),
         url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # Add url
          date.time = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%S"),
          hour = as.numeric(format(date.time, "%H")),
          created_at = as.POSIXct(created_at, format = "%Y-%m-%d")) %>%
  arrange(date.time)

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
  filter(referenced_type == "retweeted") # Remove retweets
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

# ======================================================
#           Find first activity (joining date)
# ======================================================
joining.date <- from.agency %>%
  group_by(agencyname,
           join.day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(join.day = as.POSIXct(join.day, format = "%Y-%m-%d")) %>% 
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
  dplyr::select(join.day) %>% 
  mutate(join.day = format(join.day, format = "%Y-%m-%d")) %>%
  as.character()                  
start # 2009-03-01

# ======================================================
#           Responsiveness
# ======================================================

#           Preparing dataframe
# ======================================================
to.agency <- tibble.to %>%
            unnest(cols = public_metrics, # Unnest public metrics
                  keep_empty = TRUE) %>%
            unnest(cols = attachments, # Unnest attachment
                  keep_empty = TRUE) %>%
           filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
                    in_reply_to_user_id != author_id & # Remove tweets to self
                    lang == "en") %>% # Remove non-English tweets
           mutate(text = textutils::HTMLdecode(text), # Decode html
                  url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # Add url
                  date.time = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%S"),
                  hour = as.numeric(format(date.time, "%H")),
                  created_at = as.POSIXct(created_at, format = "%Y-%m-%d")) 

# Check for duplicates
to.agency %>%
  group_by(tweet_id) %>%
  filter(n()> 1) %>%
  nrow() # No duplicates

# Create lists of tweets ids referenced by agencies
response.filter <- from.agency %>%
  mutate(referenced_id2 = paste0(referenced_id, "-", agencyname)) %>% # Add agency identifier 
  filter(!is.na(referenced_id)) %>%
  dplyr::select(referenced_id2) %>% 
  as.list()

# Create lists of tweets ids referenced by agencies (Only comments/quotes, no retweets)
response.filter.comment <- from.agency %>%
    filter(referenced_type != 'retweeted') %>%
    mutate(referenced_id2 = paste0(referenced_id, "-", agencyname)) %>% # Add agency identifier 
  filter(!is.na(referenced_id)) %>%
    dplyr::select(referenced_id2) %>% 
    as.list()

# Add joining date and referenced id with agency identifier
to.agency2  <- to.agency %>% 
  mutate(tweet_id2 = paste0(tweet_id, "-", agencyname)) %>% # Add agency identifier
  full_join(joining.date, by = "agencyname")

# Calculate length of conversations
conversation <- to.agency2 %>% group_by(conversation_id) %>%
  summarise(conversations = length(conversation_id))

# Add length of conversations
to.agency3 <- to.agency2 %>%
  full_join(conversation, by = "conversation_id") %>% 
  mutate(doc_id = row_number()) %>% # Add doc id for later
  relocate(doc_id, text) 

# Same message from same author
same.message <- to.agency3 %>%
  group_by(text, author_id) %>%
  filter(n() > 1) %>% 
  ungroup() %>%
dplyr::select(tweet_id) %>% 
  as.list()

# Uncivil

removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", " ", x, perl = T))
removeemojis <- content_transformer(function(x) gsub("[^\x01-\x7F]", " ", x, perl = T))

uncivil.data <- to.agency3 %>%
  select(doc_id, text) %>%
  as.data.frame()

corpus.uncivil <- Corpus(DataframeSource(uncivil.data)) 

corpus.tmp <- tm_map(corpus.uncivil, removeURL)
corpus.tmp <- tm_map(corpus.tmp, removeemojis)
corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
corpus.tmp <- tm_map(corpus.tmp, stemDocument)
corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)

corpus.dtm <- DocumentTermMatrix(corpus.tmp,
                                 list(dictionary = c("cunt",
                                                     "fuck",
                                                     "twat",
                                                     "stupid",
                                                     "shit",
                                                     "dick",
                                                     "tit",
                                                     "wanker",
                                                     "scumbag",
                                                     "moron",
                                                     "cock",
                                                     "foot",
                                                     "racist",
                                                     "fascist",
                                                     "sicken",
                                                     "fart",
                                                     "fuck",
                                                     "ars",
                                                     "suck",
                                                     "nigga",
                                                     "smug",
                                                     "fck", # Added
                                                     "fcking", # Added
                                                     "idiot",
                                                     "arsehol")))

uncivil.list <- corpus.dtm$i %>% as_tibble() %>%
  rename(doc_id = value) %>%
  as.list()

#           Adding other variables
# ======================================================
response <- to.agency3 %>%
  
   # Office hours
  dplyr::mutate(office.hours = case_when(
    hour >= 8 & hour < 18 ~ 1, # Code as 1 when tweet was posted between 8:00 & 18:00 CET/CEST
    TRUE ~ 0), # Code 0 if not
   
  # Did the agency respond?
  response = case_when(
    tweet_id2 %in% response.filter[["referenced_id2"]] ~ 1, # Code as 1 when tweet is AT LEAST ONCE responded to
    !tweet_id2 %in% response.filter[["referenced_id2"]] ~ 0), # Code as 0 if not
  
  # Did the agency respond with a comment?
  response.comment = case_when(
    tweet_id2 %in% response.filter.comment[["referenced_id2"]] ~ 1, # Code as 1 when tweet is AT LEAST ONCE commented on by the agency
    !tweet_id2 %in% response.filter.comment[["referenced_id2"]] ~ 0), # Code as 0 if not
  
  # Same message
  same.message = case_when(
    tweet_id %in% same.message[["tweet_id"]] ~ 1, # Code as 1 when same text from the same author exist in data
    !tweet_id %in% same.message[["tweet_id"]] ~ 0), # Code 0 if not
  
  # Uncivil tweets
    uncivil.tweet = case_when(
      doc_id %in% uncivil.list[["doc_id"]] ~ 1, # Code as 1 if uncivil
      !doc_id %in% uncivil.list[["doc_id"]] ~ 0), # Code 0 if not
  
  
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
  attachment = case_when(media_keys != "NULL" ~ 1, # To do: Check what's included in media_keys
                          TRUE ~ 0),
  
  # Real time
  real.time = difftime(strptime(created_at, format = "%Y-%m-%d"),
                   strptime(start, format = "%Y-%m-%d"), # See above
                  units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric(), # Numeric
  
  # Time on Twitter
  time.on.Twitter = difftime(strptime(created_at, format = "%Y-%m-%d"),
                       strptime(join.day, format = "%Y-%m-%d"),
                       units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric(), # Numeric

  # Year and month
  year =  format(as.POSIXct(created_at), format= "%Y"), 
  week.start = floor_date(created_at,
                          unit = "week",
                          week_start = 1), # Monday is first day of the week
  
  weighted.score = score*magnitude,

  # weekend versus weekday
         weekday = weekdays(as.POSIXct(created_at)), 
         weekend = case_when(
           weekday == "zaterdag" ~ 1, # Saturday
           weekday == "zondag" ~ 1,   # Sunday
           TRUE ~ 0))

# ======================================================
#           Constructing variables: from tweets
# ======================================================

# Create list with tweet_ids of tweets that include a mention
mentions <- from.agency %>%
  
  # Select variables
  select(c(entities, text, tweet_id, agencyhandle)) %>% 
  
  # Unnest entities and select variables
  unnest(cols = c(entities)) %>%
  select(c(mentions, text, tweet_id, agencyhandle)) %>%
  
  # Unnest mentions and keep empty rows
  unnest(cols = c(mentions)) %>%
  
  # Filter out mentions of self and create list
  mutate(agencyhandle2 = str_remove_all(agencyhandle, "@")) %>%
  filter(agencyhandle2 != username) %>%
  dplyr::select(tweet_id) %>% 
  as.list()

#           Mention/question mark
# ======================================================
engaging <- from.agency %>%
   
   # Does contain a mention?
    mutate(mention = case_when(
      tweet_id %in% mentions[["tweet_id"]] ~ 1, # Code as 1 if tweet mentions AT LEAST one user
      !tweet_id %in% mentions[["tweet_id"]] ~ 0), # Code as 0 if not
    
    # Remove handles and urls
    text.url = str_remove_all(text, "@[[:alnum:]_]{2,}"),# Remove user
    text.url = rm_url(text.url, pattern = pastex("@rm_twitter_url", "@rm_url", "rm_white_lead")),  # Remove url  
    
  # Question mark in cleaned agency tweet
  qm.agency = ifelse(grepl("\\?", text.url), 1, 0)) %>%
  select(c(mention, qm.agency, tweet_id, agencyname, referenced_type, created_at))

engaging2 <- engaging %>%
  select(mention, qm.agency, tweet_id)

#           Add new variables to response df
# ======================================================
response2 <- response %>%
  left_join(engaging2, # Left join, b/c we only want tweets TO agencies
            by = c("referenced_id" = "tweet_id")) %>%
  rename("day" = "created_at")

response2 %>%
  select(c(mention, qm.agency)) %>% 
  descr # 0.8% missing

# ======================================================
#           Joining dataframes: tweet-level
# ======================================================
response.tweet <- response2 %>%
  full_join(media.week2, by = c("agencyname", "week.start" = "week")) %>%
  dplyr::select(tweet_id, conversation_id, day, agencyname, referenced_id, url, time.on.Twitter, # General info
                response, response.comment, # DVs
                anomaly7, media, # Anomaly
                score, magnitude, weighted.score, # Sentiment
                short, weekend, retweet_count, like_count, qm.comment, attachment, year, conversations, same.message, uncivil.tweet, office.hours, # user reply controls
                qm.agency, mention, # Agency controls (with missing)
                date.time) %>% # Serial autocorrelation
  filter(time.on.Twitter >= 0)

# ======================================================
#          save data
# ======================================================
save(response.tweet, file = "response_tweet2.Rda")

# ======================================================
#           Description of key variables
# ======================================================

#          Responsiveness     
# ======================================================

# All responses
response.tweet %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Non-NAs
response.tweet %>%
  filter(!is.na(qm.agency)) %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# NAs
response.tweet %>%
  filter(is.na(qm.agency)) %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Note: higher response in df with NAs because agencies plausibly
# are more likely to respond to replies pertaining to broken links, errors, typos, etc.

# Note: Missingness is caused by tweets that no longer exist (i.e., deleted) or 
# that have not been returned by the API. 

#          Sentiment     
# ======================================================
# Distribution of sentiment
response.tweet %>%
  group_by(score) %>%
  dplyr::summarise(count = n())

response.tweet %>% select(score, weighted.score) %>%
  descr()

# Weighted score
ggplot(response.tweet, aes(x = weighted.score)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth= .25,
                 colour = "black", fill = "white")

#          Media storm     
# ======================================================
response.tweet %>%
  group_by(anomaly7) %>%
  dplyr::summarise(count = n()) %>%
  mutate(freq = count / sum(count)) 
