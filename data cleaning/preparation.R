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

<<<<<<< HEAD
# Set time zone
Sys.setenv(TZ = 'GMT')
=======
# https://stackoverflow.com/questions/57255851/sum-count-between-two-dates-in-r
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Tweets
load(file = "./data (not public)/from_tweets/from_tweets_2")
load(file = "./data (not public)/to_tweets/to_tweets_2")

# Media count
<<<<<<< HEAD
load("./data (not public)/media - outputs/media_day.Rdata")
load("./data (not public)/media - outputs/media_week.Rdata")

# Media valence
load("./data (not public)/media - outputs/media.sentiment_day.Rdata")
load("./data (not public)/media - outputs/media.sentiment_week.Rdata")
=======
load("./data (not public)/media - outputs/media_day2.Rdata")
load("./data (not public)/media - outputs/media_week2.Rdata")

# Media valence
# load("./data (not public)/media - outputs/media.sentiment_day.Rdata")
# load("./data (not public)/media - outputs/media.sentiment_week.Rdata")
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

media.day <- media.day %>%
  dplyr::rename(agencyname = acronym)

media.week <- media.week %>%
  dplyr::rename(agencyname = acronym) 

<<<<<<< HEAD
media.sentiment.day <- media.sentiment.day %>%
  dplyr::rename(agencyname = acronym)  %>%
  mutate(day = round(as.POSIXct(day), "day")) # Remove this line when new sentiment analysis is available

media.sentiment.week <- media.sentiment.week %>%
  dplyr::rename(agencyname = acronym) %>%
  mutate(week = round(as.POSIXct(week), "day")) # Remove this line when new sentiment analysis is available

=======
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
# ======================================================
#           Information
# ======================================================

from.agency <- tibble.from %>%
<<<<<<< HEAD
 filter(agencyname != "FRA") %>% # To do: add FRA
 arrange(referenced_type != 'replied_to') %>% # Remove 'quoted' if 'replied to' is present for same tweet id
  distinct(tweet_id, .keep_all = TRUE) %>%
  mutate(url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # Add url
         created_at = round(with_tz(as.Date(created_at),  "GMT"),  "days"),
         text = textutils::HTMLdecode(text)) # Decode html
=======
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
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

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
<<<<<<< HEAD
  mutate(day = as.POSIXct(day))
=======
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Information per week
information.week <- information %>%
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
<<<<<<< HEAD
  mutate(week = as.POSIXct(week))
=======
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"))
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# ======================================================
#           Find first activity (joining date)
# ======================================================

joining.date <- from.agency %>%
  group_by(agencyname,
           join.day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
<<<<<<< HEAD
  mutate(join.day = as.POSIXct(join.day)) %>% 
=======
  mutate(join.day = as.POSIXct(join.day, format = "%Y-%m-%d")) %>% 
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
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
<<<<<<< HEAD
  mutate(join.day = format(join.day, "%Y-%m-%d")) %>%
=======
  mutate(join.day = format(join.day, format = "%Y-%m-%d")) %>%
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
  as.character()                  

# ======================================================
#           Responsiveness
# ======================================================

#           Preparing dataframe
# ======================================================
<<<<<<< HEAD

to.agency <- tibble.to %>%
  filter(agencyname != "FRA") %>% # To do: add FRA
  unnest(cols = attachments, # Unnest attachment
                  keep_empty = TRUE) %>%
  filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
           in_reply_to_user_id != author_id & # Remove tweets to self
           lang == "en") %>% # Remove non-English tweets
  mutate(created_at = as.POSIXct(created_at),
         text = textutils::HTMLdecode(text), # Decode html
         url = paste0("www.twitter.com/", author_id, "/status/", tweet_id))  # Add url
=======
to.agency <- tibble.to %>%
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

# Add count of comments in past 7 days
# interval <- to.agency.t %>%
#  select(tweet_id,
#         week.earlier,
#         date.time,
#         agencyname) %>%
#  arrange(date.time) %>%
#  drop_na(date.time)

# setDT(interval)
# interval[, n:= 1:.N - findInterval(week.earlier, date.time),
#         by=.(agencyname)]

# to.agency <- to.agency.t %>%
#  full_join((interval %>%
#               select(tweet_id, n)),
#               by = "tweet_id") %>%
#                    rename(seven.days = n)
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

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

<<<<<<< HEAD
=======
to.agency3$hours

>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
#           Adding other variables
# ======================================================
response <- to.agency3 %>%
  
<<<<<<< HEAD
  # Same message
  dplyr::mutate(same.message = case_when(
=======
  # Office hours
  dplyr::mutate(office.hours = case_when(
    hour >= 8 & hour < 18 ~ 1, # Code as 1 when tweet was posted between 8:00 & 18:00 CET/CEST
    TRUE ~ 0), # Code 0 if not
    
  # Same message
  same.message = case_when(
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
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
  
<<<<<<< HEAD
  
  # Did the agency respond with a comment?
=======
    # Did the agency respond with a comment?
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
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
<<<<<<< HEAD
  
  # ALL CAPS
  cap = stringi::stri_count_regex(text.url, grab("@rm_caps")),
  cap.perc = cap/ngrams,
  cap.dummy = case_when(
           cap.perc >= 0.5 ~ 1,
           cap.perc < 0.5 ~ 0),
  
  # Year and month
  year =  format(as.POSIXct(created_at), format= "%Y"), 
  month =  format(as.POSIXct(created_at), format= "%m-%Y"),
  
=======
 
  # Year and month
  year =  format(as.POSIXct(created_at), format= "%Y"), 

>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
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
  descr # 0.87% missing


#           count comments/responses per day
# ======================================================
responsiveness.day <- response2 %>%
  group_by(agencyname,
           day = cut(day, "day"),
           .drop = FALSE) %>%
  dplyr::summarise(comments.count = n(),
                   responses.count = sum(response)) %>%
<<<<<<< HEAD
  mutate(day = as.POSIXct(day))
=======
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

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
  mutate(response = case_when(
    response == 1 ~ "response",
    response == 0 ~ "no.response")) %>%
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
  group_by(response, agencyname, day) %>%
  mutate(response = case_when(
    response == 1 ~ "response",
    response == 0 ~ "no.response")) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  mutate(halfyear = as.Date(lubridate::floor_date(day, unit = "halfyear"))) %>%
  group_by(halfyear, agencyname) %>%            # Group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  group_by(agencyname) %>% 
  mutate(max = case_when(
    max(no.response) > max(response) ~ max(no.response),
    max(response) > max(no.response) ~ max(response)),
        response.rate = paste0(round(sum(response)/(sum(response) + sum(no.response)) * 100, 2), "%")) %>% 
  ungroup() %>%
  arrange(max) %>%               # Sort dataframe
  mutate(agencyname = factor(agencyname, unique(agencyname))) # Reset factor-column based on that order

# Creating a plot 
response.plot %>%
  ggplot(aes(x = halfyear)) +  
  facet_wrap(facets = . ~ agencyname,  nrow = 9, scales = "free_y") +
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

sentiment.data <- merge(sentiment,
                      response2,
                      by.y = "text",
                      by.x = "text") %>% # Merge scores with data
  distinct() %>% 
  mutate(sentiment = case_when(
    compound <= -0.05 ~ "twitter.criticism",
    compound > -0.05 & compound < 0.05 ~ "twitter.neutral",
    compound >= 0.05 ~ "twitter.praise")) 

# Distribution
sentiment.data %>%
  group_by(sentiment) %>%
  dplyr::summarise(count = n())

# Consequences per day
consequences.day <- sentiment.data %>%
  dplyr::group_by(agencyname, day = cut(day, "day"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
<<<<<<< HEAD
  mutate(day = as.POSIXct(day))
=======
  mutate(day = as.POSIXct(day, format = "%Y-%m-%d"))
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Consequences per week
consequences.week <- sentiment.data %>%
  dplyr::group_by(agencyname, week = cut(day, "week"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
<<<<<<< HEAD
  mutate(week = as.POSIXct(week),
=======
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
         twitter.index = twitter.praise - twitter.criticism)

# ======================================================
#           Joining dataframes: tweet-level
# ======================================================
# Join dataframes with daily data
twitter.day <- information.day %>% 
  full_join(consequences.day,  by = c("agencyname", "day")) %>%
  full_join(media.day, by = c("agencyname", "day")) %>%
<<<<<<< HEAD
  full_join(media.sentiment.day,  by = c("agencyname", "day")) %>%
=======
 # full_join(media.sentiment.day,  by = c("agencyname", "day")) %>%
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
  full_join(responsiveness.day,  by = c("agencyname", "day")) %>%
  full_join(joining.date, by = "agencyname") %>%
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  
# Twitter vars to NA if agency wasn't on Twitter
  mutate_at(.vars = c("information",
                      "comments.count",
                      "responses.count",
                      "twitter.praise",
                      "twitter.neutral",
                      "twitter.criticism"),
            funs(ifelse(day < join.day, NA, .)))

#           Rolling sum/lags
# ======================================================
twitter.day.lag <- twitter.day %>%
  arrange(day) %>% # Arrange by day
  group_by(agencyname) %>%
  dplyr::mutate(
    # Twitter
    
    # Number of responses in past week/month/quarter/year
    responses.1w = rollapplyr(responses.count, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    responses.30d = rollapplyr(responses.count, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    responses.90d = rollapplyr(responses.count, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    responses.365d = rollapplyr(responses.count, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    
    # Number of comments in past week/month/quarter/year
    comments.1w = rollapplyr(comments.count, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    comments.30d = rollapplyr(comments.count, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    comments.90d = rollapplyr(comments.count, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    comments.365d = rollapplyr(comments.count, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    
    # ratio between responses and comments in past week/month/quarter/year
    responses.ratio.1w = ifelse(!comments.1w, 0, (responses.1w / comments.1w)),
    responses.ratio.30d = ifelse(!comments.30d, 0, (responses.30d / comments.30d)),
    responses.ratio.90d = ifelse(!comments.90d, 0, (responses.90d / comments.90d)),
    responses.ratio.365d = ifelse(!comments.365d, 0, (responses.365d / comments.365d)),
    
    # Valence on Twitter
    # Valence on Twitter in past week
    twitter.criticism.1w = rollapplyr(twitter.criticism, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    twitter.praise.1w = rollapplyr(twitter.praise, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.1w = rollapplyr(twitter.neutral, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    twitter.valence.1w = ifelse(!(twitter.praise.1w + twitter.neutral.1w + twitter.criticism.1w), 0, (twitter.praise.1w - twitter.criticism.1w) / (twitter.praise.1w + twitter.neutral.1w + twitter.criticism.1w)),
    
    # Meijer and Kleinnijenhuis index (1 week): pos - neg
    twitter.index.1w = twitter.praise.1w - twitter.criticism.1w,
    
        # Valence on Twitter in past 30 days
    twitter.criticism.30d = rollapplyr(twitter.criticism, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.praise.30d = rollapplyr(twitter.praise, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.30d = rollapplyr(twitter.neutral, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.valence.30d = ifelse(!(twitter.praise.30d + twitter.neutral.30d + twitter.criticism.30d), 0, (twitter.praise.30d - twitter.criticism.30d) / (twitter.praise.30d + twitter.neutral.30d + twitter.criticism.30d)),
    
    # Meijer and Kleinnijenhuis index (30 days): pos - neg
    twitter.index.30d = twitter.praise.30d - twitter.criticism.30d,
    
    # Valence on Twitter in past 90 days
    twitter.criticism.90d = rollapplyr(twitter.criticism, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.praise.90d = rollapplyr(twitter.praise, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.90d = rollapplyr(twitter.neutral, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.valence.90d = ifelse(!(twitter.praise.90d + twitter.neutral.90d + twitter.criticism.90d), 0, (twitter.praise.90d - twitter.criticism.90d) / (twitter.praise.90d + twitter.neutral.90d + twitter.criticism.90d)),
    
    # Meijer and Kleinnijenhuis index (90 days): pos - neg
    twitter.index.90d = twitter.praise.90d - twitter.criticism.90d,
    
    # Valence on Twitter in past 365 days
    twitter.criticism.365d = rollapplyr(twitter.criticism, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.praise.365d = rollapplyr(twitter.praise, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.365d = rollapplyr(twitter.neutral, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.valence.365d = ifelse(!(twitter.praise.365d + twitter.neutral.365d + twitter.criticism.365d), 0, (twitter.praise.365d - twitter.criticism.365d) / (twitter.praise.365d + twitter.neutral.365d + twitter.criticism.365d)),
    
    # Meijer and Kleinnijenhuis index (365 days): pos - neg
    twitter.index.365d = twitter.praise.365d - twitter.criticism.365d,
    
    # Media
    # Valence in media in past 30 days
<<<<<<< HEAD
    criticism.30d = rollapplyr(negative, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    praise.30d = rollapplyr(positive, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    neutral.30d = rollapplyr(neutral, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    media.valence.30d = ifelse(!(praise.30d + neutral.30d + criticism.30d), 0, (praise.30d - criticism.30d) / (praise.30d + neutral.30d + criticism.30d)),
    
    # Valence in media in past 90 days
    criticism.90d = rollapplyr(negative, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    praise.90d = rollapplyr(positive, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    neutral.90d = rollapplyr(neutral, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    media.valence.90d = ifelse(!(praise.90d + neutral.90d + criticism.90d), 0, (praise.90d - criticism.90d) / (praise.90d + neutral.90d + criticism.90d)),
    
    # Valence in media in past 365 days
    criticism.365d = rollapplyr(negative, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    praise.365d = rollapplyr(positive, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    neutral.365d = rollapplyr(neutral, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    media.valence.365d = ifelse(!(praise.365d + neutral.365d + criticism.365d), 0, (praise.365d - criticism.365d) / (praise.365d + neutral.365d + criticism.365d)),
    
    # Media count
=======
   # criticism.30d = rollapplyr(negative, list(seq(-30, -1)), sum, fill = NA, align = "right"),
   # praise.30d = rollapplyr(positive, list(seq(-30, -1)), sum, fill = NA, align = "right"),
   # neutral.30d = rollapplyr(neutral, list(seq(-30, -1)), sum, fill = NA, align = "right"),
   # media.valence.30d = ifelse(!(praise.30d + neutral.30d + criticism.30d), 0, (praise.30d - criticism.30d) / (praise.30d + neutral.30d + criticism.30d)),
    
    # Valence in media in past 90 days
   # criticism.90d = rollapplyr(negative, list(seq(-90, -1)), sum, fill = NA, align = "right"),
   #  praise.90d = rollapplyr(positive, list(seq(-90, -1)), sum, fill = NA, align = "right"),
   # neutral.90d = rollapplyr(neutral, list(seq(-90, -1)), sum, fill = NA, align = "right"),
   # media.valence.90d = ifelse(!(praise.90d + neutral.90d + criticism.90d), 0, (praise.90d - criticism.90d) / (praise.90d + neutral.90d + criticism.90d)),
    
    # Valence in media in past 365 days
   # criticism.365d = rollapplyr(negative, list(seq(-365, -1)), sum, fill = NA, align = "right"),
   # praise.365d = rollapplyr(positive, list(seq(-365, -1)), sum, fill = NA, align = "right"),
   # neutral.365d = rollapplyr(neutral, list(seq(-365, -1)), sum, fill = NA, align = "right"),
   # media.valence.365d = ifelse(!(praise.365d + neutral.365d + criticism.365d), 0, (praise.365d - criticism.365d) / (praise.365d + neutral.365d + criticism.365d)),
    
    # Media count (source)
    media.count.1w = rollapplyr(media.source, list(seq(-7, -1)), sum, fill = NA, align = "right"),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
    media.count.30d = rollapplyr(media.source, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    media.count.90d = rollapplyr(media.source, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    media.count.365d = rollapplyr(media.source, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    
<<<<<<< HEAD
    # Information
    information.1w = rollapplyr(information, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    information.30d = rollapplyr(information, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    information.90d = rollapplyr(information, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    information.365d = rollapplyr(information, list(seq(-365, -1)), sum, fill = NA, align = "right"))

# Select variable and merge with lagged data
response.tweet <- sentiment.data %>%
  dplyr::select(tweet_id, conversation_id, day, agencyname, response, short, weekend,
         response, qm.comment, attachment, real.time, year, month, sentiment, referenced_id,
         time.on.Twitter, mention, qm.agency, conversations, url, same.message) %>%
  left_join(twitter.day.lag, by = c("agencyname", 'day')) %>%
  filter(time.on.Twitter >= 0)
=======
   # Media count (begin)
   media.begin.1w = rollapplyr(media.begin, list(seq(-7, -1)), sum, fill = NA, align = "right"),
   media.begin.30d = rollapplyr(media.begin, list(seq(-30, -1)), sum, fill = NA, align = "right"),
   media.begin.90d = rollapplyr(media.begin, list(seq(-90, -1)), sum, fill = NA, align = "right"),
   media.begin.365d = rollapplyr(media.begin, list(seq(-365, -1)), sum, fill = NA, align = "right"))
   
# Select variable and merge with lagged data
response.tweet <- sentiment.data %>%
  dplyr::select(tweet_id, conversation_id, day, agencyname, response, short, weekend,
         qm.comment, attachment, real.time, year, sentiment, referenced_id,
         time.on.Twitter, mention, qm.agency, conversations, url, same.message, uncivil.tweet, office.hours) %>%
  left_join(twitter.day.lag, by = c("agencyname", 'day')) %>%
  filter(time.on.Twitter >= 0) %>%
  ungroup()
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# ======================================================
#           Joining dataframes: agency-week panel
# ======================================================

# Join dataframes with weekly data
twitter.week <- information.week %>%
  full_join(consequences.week, by = c("agencyname", "week")) %>%
  full_join(media.week, by = c("agencyname", "week")) %>%
<<<<<<< HEAD
  full_join(media.sentiment.week,  by = c("agencyname", "week")) %>%
=======
 # full_join(media.sentiment.week,  by = c("agencyname", "week")) %>%
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
  full_join(joining.date, by = "agencyname")

#           From agency variables
# ======================================================
agency.week <- engaging %>%
  filter(referenced_type != 'retweeted') %>% # Remove RTs because lenght/mention is not decided by agency
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  dplyr::summarize(mention.week = sum(mention),
            qm.agency.week = sum(qm.agency),
            no.of.tweets = n()) %>%
<<<<<<< HEAD
  mutate(week = as.POSIXct(week),
=======
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
         mention.ratio = ifelse(!(no.of.tweets), 0, (mention.week/no.of.tweets)),
         qm.agency.ratio = ifelse(!(no.of.tweets), 0, (qm.agency.week/no.of.tweets)))

#           Putting it all together
# ======================================================
response.panel <- sentiment.data %>%
<<<<<<< HEAD
  filter(same.message == 0) %>% # Remove 'same message' tweets
=======
 # filter(same.message == 0) %>% # Remove 'same message' tweets?
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
  group_by(agencyname,
           week = cut(day, "week"),
           .drop = FALSE) %>%
  dplyr::summarise(offset.week = n(),
            response.week = sum(response),
            same.message.week = sum(same.message),
            attachment.week = sum(attachment),
            qm.comment.week = sum(qm.comment),
            weekend.week = sum(weekend),
            short.week = sum(short))  %>%
<<<<<<< HEAD
  mutate(week = as.POSIXct(week),
=======
  mutate(week = as.POSIXct(week, format = "%Y-%m-%d"),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
         attachment.ratio = ifelse(!(offset.week), 0, (attachment.week/offset.week)),
         same.message.ratio = ifelse(!(offset.week), 0, (same.message.week/offset.week)),
         qm.comment.ratio = ifelse(!(offset.week), 0, (qm.comment.week/offset.week)),
         short.ratio = ifelse(!(offset.week), 0, (short.week/offset.week)),
         weekend.ratio =  ifelse(!(offset.week), 0, (weekend.week/offset.week))) %>%
  left_join(twitter.week, by = c("agencyname", 'week')) %>% 
  left_join(agency.week, by = c("agencyname", 'week')) %>% 
  mutate(time.on.Twitter = difftime(strptime(week, format = "%Y-%m-%d"),
                             strptime(join.day, format = "%Y-%m-%d"), units = c("weeks")) %>% as.numeric(),
<<<<<<< HEAD
         real.time = difftime(strptime(created_at, format = "%Y-%m-%d"),
                              strptime(start, format = "%Y-%m-%d"), # See above
                              units = c("days")),
         twitter.index = twitter.praise - twitter.criticism,
         media.valence = ifelse(!(positive + neutral + negative), 0, (positive - negative) / (positive + neutral + negative))) %>%
  filter(time.on.Twitter >= 0)
=======
         real.time = difftime(strptime(week, format = "%Y-%m-%d"),
                              strptime(start, format = "%Y-%m-%d"), # See above
                              units = c("days")),
         twitter.index = twitter.praise - twitter.criticism) %>%
  arrange(week) %>%
  group_by(agencyname) %>% # Lag by one week
  mutate(twitter.criticism.1w = rollapplyr(twitter.criticism, list(seq(-1, -1)), sum, fill = NA, align = "right"),
       twitter.praise.1w = rollapplyr(twitter.praise, list(seq(-1, -1)), sum, fill = NA, align = "right"),
       twitter.neutral.1w = rollapplyr(twitter.neutral, list(seq(-1, -1)), sum, fill = NA, align = "right"),
       twitter.valence1 = ifelse(!(twitter.praise.1w + twitter.neutral.1w + twitter.criticism.1w), 0, (twitter.praise.1w - twitter.criticism.1w) / (twitter.praise.1w + twitter.neutral.1w + twitter.criticism.1w)),

       twitter.index1 = rollapplyr(twitter.index, list(seq(-1, -1)), sum, fill = NA, align = "right"),
       media.source1 = rollapplyr(media.source, list(seq(-1, -1)), sum, fill = NA, align = "right"),
       media.begin1 = rollapplyr(media.begin, list(seq(-1, -1)), sum, fill = NA, align = "right")) %>%
        filter(time.on.Twitter >= 0)
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

#           A look at the distribution
# ======================================================
response.panel %>% 
  filter(offset.week != 0) %>%
  group_by(response.week) %>%
  summarise(n = n())

# ======================================================
#          save data
# ======================================================
<<<<<<< HEAD
save(response.tweet, file = "response_tweet.Rda")
save(response.panel, file = "response_panel.Rda")
=======
save(response.tweet, file = "response_tweet2.Rda")
save(response.panel, file = "response_panel2.Rda")
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
