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
library(stringi)
library(tm)
library(qdapRegex)

# Set time zone
Sys.setenv()

# Tweets
load(file = "./data (not public)/from_tweets/from_tweets_3")
load(file = "./data (not public)/to_tweets/tweetsTO_final.RData")

# Media count
load("./data (not public)/media - outputs/media_week2_new.Rdata")

media.week2 <- media.week2 %>%
  dplyr::rename(agencyname = acronym) 

# ======================================================
#           Information
# ======================================================
from.agency <- tibble.from %>%
  arrange(referenced_type != 'replied_to') %>% # Remove 'quoted' if 'replied to' is present for same tweet id
  distinct(tweet_id, .keep_all = TRUE) %>%
  mutate(text = textutils::HTMLdecode(text),
         url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # Add url
          date.time = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%S"),
          hour = as.numeric(format(date.time, "%H")),
          created_at = as.POSIXct(created_at, format = "%Y-%m-%d"),
          week.earlier = date.time - weeks(1),
          interval = interval(week.earlier, date.time)) %>%
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

# Information per day
information.day <- information %>%
   group_by(agencyname,
            day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))

# Information per week
information.week <- information %>%
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"))

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
  dplyr::select(join.day) %>% # 2009-03-01
  mutate(join.day = format(join.day, format = "%Y-%m-%d")) %>%
  as.character()                  

# ======================================================
#           Responsiveness
# ======================================================

#           Preparing dataframe
# ======================================================
to.agency <- tweetsTO_final %>%
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
                  created_at = as.POSIXct(created_at, format = "%Y-%m-%d"),
                  week.earlier = date.time - weeks(1),
                  interval = interval(week.earlier, date.time)) 

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

#           Adding joining date and conversation length
# ======================================================
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

uncivil.data <- to.agency3 %>% select(doc_id, text) 
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
    
  # Same message
  same.message = case_when(
    tweet_id %in% same.message[["tweet_id"]] ~ 1, # Code as 1 when same text from the same author exist in data
    !tweet_id %in% same.message[["tweet_id"]] ~ 0), # Code 0 if not

  # Did the agency respond?
  response = case_when(
    tweet_id2 %in% response.filter[["referenced_id2"]] ~ 1, # Code as 1 when tweet is AT LEAST ONCE responded to
    !tweet_id2 %in% response.filter[["referenced_id2"]] ~ 0), # Code as 0 if not
  
  # Uncivil tweets
    uncivil.tweet = case_when(
      doc_id %in% uncivil.list[["doc_id"]] ~ 1, # Code as 1 if uncivil
      !doc_id %in% uncivil.list[["doc_id"]] ~ 0), # Code 0 if not
  
     # Did the agency respond with a comment?
  response.comment = case_when(
    tweet_id2 %in% response.filter.comment[["referenced_id2"]] ~ 1, # Code as 1 when tweet is AT LEAST ONCE commented on by the agency
    !tweet_id2 %in% response.filter.comment[["referenced_id2"]] ~ 0), # Code as 0 if not
 
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
  month =  format(as.POSIXct(created_at), format= "%m-%Y"),
  week.start = floor_date(created_at, "%m/%d/%Y", unit = "week"),
  
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

#           count comments/responses per day
# ======================================================
responsiveness.day <- response2 %>%
  group_by(agencyname,
           day = cut(day, "day"),
           .drop = FALSE) %>%
  dplyr::summarise(comments.count = n(),
                   responses.count = sum(response)) %>%
  mutate(day = as.POSIXct(day))
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))

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

# Distribution of consequences
response2 %>%
  group_by(score) %>%
  dplyr::summarise(count = n())

response2 %>% select(score) %>%
  descr()

# Consequences per day
consequences <- response2 %>%
  drop_na(score) %>%
  mutate(score.chr = case_when(
  # score == -1 ~ "min1", # No scores of -1 in dataset
    score == -0.9 ~ "min09",
    score == -0.8 ~ "min08",
    score == -0.7 ~ "min07",
    score == -0.6 ~ "min06",
    score == -0.5 ~ "min05",
    score == -0.4 ~ "min04",
    score == -0.3 ~ "min03",
    score == -0.2  ~ "min02",
    score == -0.1 ~ "min01",
    score == 0 ~ "neutral",
    score == 0.1 ~ "plus01",
    score == 0.2 ~ "plus02",
    score == 0.3 ~ "plus03",
    score == 0.4 ~ "plus04",
    score == 0.5 ~ "plus05",
    score == 0.6 ~ "plus06",
    score == 0.7 ~ "plus07",
    score == 0.8 ~ "plus08",
    score == 0.9 ~ "plus09",
  #  score == 1 ~ "plus1", # No scores of 1 in dataset
    )) 

consequences.day <- consequences %>%
  dplyr::group_by(agencyname, day = cut(day, "day"),  .drop = FALSE)  %>% 
  dplyr::count(score.chr) %>%
  pivot_wider(names_from = "score.chr",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))

# Consequences per week
consequences.week <- consequences %>%
  dplyr::group_by(agencyname, week = cut(day, "week"),  .drop = FALSE)  %>% 
  dplyr::count(score.chr) %>%
  pivot_wider(names_from = "score.chr",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"),
         twitter.index = 0.9*plus09 +
                         0.8*plus08 +
                         0.7*plus07 +
                         0.6*plus06 +
                         0.5*plus05 +
                         0.4*plus04 +
                         0.3*plus03 +
                         0.2*plus02 +
                         0.1*plus01 -
                         0.1*min01 -
                         0.2*min02 -
                         0.3*min03 -
                         0.4*min04 -
                         0.5*min05 -
                         0.6*min06 -
                         0.7*min07 -
                         0.8*min08 -
                         0.9*min09,
         twitter.index = twitter.praise - twitter.criticism)

# ======================================================
#           Joining dataframes: tweet-level
# ======================================================
# Join dataframes with daily data
twitter.day <- information.day %>% 
  full_join(consequences.day,  by = c("agencyname", "day")) %>%
  
  # Check code! "week.start"
  full_join(media.week2, by = c("agencyname", "week.start")) %>%
  
  full_join(responsiveness.day, by = c("agencyname", "day")) %>%
  full_join(joining.date, by = "agencyname") %>%
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  
# Twitter vars to NA if agency wasn't on Twitter
  mutate_at(.vars = c("information",
                      "comments.count",
                      "responses.count"),
            funs(ifelse(day < join.day, NA, .)))

#           Rolling sum/lags
# ======================================================
twitter.day.lag <- twitter.day %>%
  arrange(day) %>% # Arrange by day
  group_by(agencyname) %>%
  dplyr::mutate(
    # Twitter
    
    # Number of responses in past 7 days
    responses.1w = rollapplyr(responses.count, list(seq(-7, -1)), sum, fill = NA, align = "right"),

    # Number of comments in past 7 days
    comments.1w = rollapplyr(comments.count, list(seq(-7, -1)), sum, fill = NA, align = "right"),

    # Valence on Twitter in past 7 days
    min09.1w = rollapplyr(min09, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min08.1w = rollapplyr(min08, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min07.1w = rollapplyr(min07, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min06.1w = rollapplyr(min06, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min05.1w = rollapplyr(min05, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min04.1w = rollapplyr(min04, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min03.1w = rollapplyr(min03, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min02.1w = rollapplyr(min02, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    min01.1w = rollapplyr(min01, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    
    neutral.1w = rollapplyr(neutral, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    
    plus09.1w = rollapplyr(plus09, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus08.1w = rollapplyr(plus08, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus07.1w = rollapplyr(plus07, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus06.1w = rollapplyr(plus06, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus05.1w = rollapplyr(plus05, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus04.1w = rollapplyr(plus04, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus03.1w = rollapplyr(plus03, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus02.1w = rollapplyr(plus02, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    plus01.1w = rollapplyr(plus01, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    
    twitter.index.1w = 0.9*plus09.1w +
                      0.8*plus08.1w +
                      0.7*plus07.1w +
                      0.6*plus06.1w +
                      0.5*plus05.1w +
                      0.4*plus04.1w +
                      0.3*plus03.1w +
                      0.2*plus02.1w +
                      0.1*plus01.1w -
                      0.1*min01.1w -
                      0.2*min02.1w -
                      0.3*min03.1w -
                      0.4*min04.1w -
                      0.5*min05.1w -
                      0.6*min06.1w -
                      0.7*min07.1w -
                      0.8*min08.1w -
                      0.9*min09.1w,
        twitter.valence.1w = ifelse(!(comments.1w),
                                0,
                                (twitter.index.1w) / (comments.1w)),

    # Information in past 7 days
    information.1w = rollapplyr(information, list(seq(-7, -1)), sum, fill = NA, align = "right"))

# Select variable and merge with lagged data
response.tweet <- response2 %>%
  dplyr::select(tweet_id, conversation_id, day, agencyname, response, short, weekend, retweet_count, like_count,
         qm.comment, attachment, real.time, year, score, referenced_id, 
         time.on.Twitter, mention, qm.agency, conversations, url, same.message, uncivil.tweet, office.hours, weighted.score) %>%
  left_join(twitter.day.lag, by = c("agencyname", 'day')) %>%
  filter(time.on.Twitter >= 0) %>%
  ungroup()

# ======================================================
#           Joining dataframes: agency-week panel
# ======================================================

# Join dataframes with weekly data
twitter.week <- information.week %>%
  full_join(consequences.week, by = c("agencyname", "week")) %>%
  full_join(media.week2, by = c("agencyname", "week")) %>%
  full_join(joining.date, by = "agencyname")

#           Putting it all together
# ======================================================
response.panel <- sentiment.data %>%
  group_by(agencyname,
           week = cut(day, "week"),
           .drop = FALSE) %>%
  dplyr::summarise(offset.week = n(),
                   response.week = sum(response))  %>%
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d")) %>%
  select(c(week, offset.week, response.week, agencyname))
  left_join(twitter.week, by = c("agencyname", 'week')) %>% 
  mutate(time.on.Twitter = difftime(strptime(week, format = "%Y-%m-%d"),
                             strptime(join.day, format = "%Y-%m-%d"), units = c("weeks")) %>% as.numeric(),
         real.time = difftime(strptime(week, format = "%Y-%m-%d"),
                              strptime(start, format = "%Y-%m-%d"), # See above
                              units = c("days")),
         twitter.valence = ifelse(!(offset.week),
                                  0,
                                  (twitter.index) / (offset.week))) %>%
  arrange(week) %>%
  group_by(agencyname) %>% # Lag by one week
  mutate(twitter.index1 = rollapplyr(twitter.index, list(seq(-1, -1)), sum, fill = NA, align = "right")) %>%
        filter(time.on.Twitter >= 0)

#           A look at the distribution
# ======================================================
response.panel %>% 
  filter(offset.week != 0) %>%
  group_by(response.week) %>%
  summarise(n = n())

# ======================================================
#          save data
# ======================================================
save(response.tweet, file = "response_tweet2.Rda")
save(response.panel, file = "response_panel2.Rda")
