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

# To do's:
## Check negative time on Twitter
# Count and sentiment of media variables doesn't add up (probably due to rounding)
# Add other measure for reply.count.week variable based on own data
# Check if all weeks are present in both panel data frame

# Tweets
load(file = "./data (not public)/from_tweets/from_tweets_2")
load(file = "./data (not public)/to_tweets/to_tweets_2")

# Media count
load("./data (not public)/media - outputs/media_day.Rdata")
load("./data (not public)/media - outputs/media_week.Rdata")

media.day <- media.day %>%
  dplyr::rename(agencyname = acronym)
media.week <- media.week %>%
  dplyr::rename(agencyname = acronym)
media.sentiment.day <- media.sentiment.day %>%
  dplyr::rename(agencyname = acronym)
media.sentiment.week <- media.sentiment.week %>%
  dplyr::rename(agencyname = acronym)

# Media valence

load("./data (not public)/media - outputs/media.sentiment_day.Rdata")
load("./data (not public)/media - outputs/media.sentiment_week.Rdata")

# ======================================================
#           Information
# ======================================================

from.agency <- tibble.from %>%
 filter(agencyname != "FRA") %>% # To do: add FRA
 arrange(referenced_type != 'replied_to') %>% # Remove 'quoted' if 'replied to' is present for same tweet id
  distinct(tweet_id, .keep_all = TRUE) %>%
  mutate(url = paste0("www.twitter.com/", author_id, "/status/", tweet_id),  # Add url
         created_at = as.Date(created_at),
         text = textutils::HTMLdecode(text)) # Decode html

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
  mutate(day = round(as.POSIXct(day), "day"))

# Information per week
information.week <- information %>%
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(week = round(as.POSIXct(week), "day"))

# ======================================================
#           Find first activity (joining date)
# ======================================================

joining.date <- from.agency %>%
  group_by(agencyname,
           join.day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(join.day = round(as.POSIXct(join.day), "day")) %>% 
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
  filter(agencyname != "FRA") %>% # To do: add FRA
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
to.agency3 <- to.agency2 %>%
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
  attachment = case_when(media_keys != "NULL" ~ 1, # To do: Check what's included in media_keys
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
            by = c("referenced_id" = "tweet_id")) %>%
  rename("day" = "created_at")

response2 %>%
  select(c(mention, qm.agency)) %>% 
  descr # 0.87% missing

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
  group_by(response, agencyname, day) %>%
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
  distinct() %>% # TO DO: Why are duplicates created?
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
  mutate(day = round(as.POSIXct(day), "day"))

# Consequences per week
consequences.week <- sentiment.data %>%
  dplyr::group_by(agencyname, week = cut(day, "week"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(week = round(as.POSIXct(week), "day"))

# ======================================================
#           Joining dataframes: tweet-level
# ======================================================
# Join dataframes with daily data
twitter.day <- information.day %>%
  full_join(consequences.day,  by = c("agencyname", "day")) %>%
  full_join(media.day, by = c("agencyname", "day")) %>%
  full_join(media.sentiment.day,  by = c("agencyname", "day")) %>%
  full_join(joining.date, by = "agencyname") %>%
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  
# Twitter vars to NA if agency wasn't on Twitter
  mutate_at(.vars = c("information", 
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
    # Valence on Twitter in past 30 days
    twitter.criticism.30d = rollapplyr(twitter.criticism, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.praise.30d = rollapplyr(twitter.praise, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.30d = rollapplyr(twitter.neutral, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    twitter.valence.30d = ifelse(!(twitter.praise.30d + twitter.neutral.30d + twitter.criticism.30d), 0, (twitter.praise.30d - twitter.criticism.30d) / (twitter.praise.30d + twitter.neutral.30d + twitter.criticism.30d)),
    
    # Valence on Twitter in past 90 days
    twitter.criticism.90d = rollapplyr(twitter.criticism, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.praise.90d = rollapplyr(twitter.praise, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.90d = rollapplyr(twitter.neutral, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    twitter.valence.90d = ifelse(!(twitter.praise.90d + twitter.neutral.90d + twitter.criticism.90d), 0, (twitter.praise.90d - twitter.criticism.90d) / (twitter.praise.90d + twitter.neutral.90d + twitter.criticism.90d)),
    
    # Valence on Twitter in past 365 days
    twitter.criticism.365d = rollapplyr(twitter.criticism, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.praise.365d = rollapplyr(twitter.praise, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.neutral.365d = rollapplyr(twitter.neutral, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    twitter.valence.365d = ifelse(!(twitter.praise.365d + twitter.neutral.365d + twitter.criticism.365d), 0, (twitter.praise.365d - twitter.criticism.365d) / (twitter.praise.365d + twitter.neutral.365d + twitter.criticism.365d)),
    
    # Media
    # Valence in media in past 30 days
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
    media.count.30d = rollapplyr(media, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    media.count.90d = rollapplyr(media, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    media.count.365d = rollapplyr(media, list(seq(-365, -1)), sum, fill = NA, align = "right"),
    
    # Information
    information.1w = rollapplyr(information, list(seq(-7, -1)), sum, fill = NA, align = "right"),
    information.30d = rollapplyr(information, list(seq(-30, -1)), sum, fill = NA, align = "right"),
    information.90d = rollapplyr(information, list(seq(-90, -1)), sum, fill = NA, align = "right"),
    information.365d = rollapplyr(information, list(seq(-365, -1)), sum, fill = NA, align = "right"))

# Select variable and merge with lagged data
response.tweet <- sentiment.data %>%
  select(tweet_id, conversation_id, day, agencyname, response, short, 
         response.num, qm.comment, attachment, real.time, year, sentiment,
         time.on.Twitter, mention, qm.agency, conversations, url) %>%
  left_join(twitter.day.lag, by = c("agencyname", 'day'))

# ======================================================
#           Joining dataframes: agency-week panel
# ======================================================

# Join dataframes with daily data
twitter.week <- information.week %>%
  full_join(consequences.week,  by = c("agencyname", "week")) %>%
  full_join(media.week, by = c("agencyname", "week")) %>%
  full_join(media.sentiment.week,  by = c("agencyname", "week")) %>%
  full_join(joining.date, by = "agencyname")

# From agency variables
agency.week <- engaging %>%
  full_join(from.agency, 
            by = c("tweet_id")) %>%
  unnest(cols = public_metrics, # Unnest attachment
         keep_empty = TRUE) %>%
  group_by(agencyname,
           week = cut(created_at, "week"),
           .drop = FALSE) %>%
  dplyr::summarize(information.week = n(),
            mention.week = sum(mention),
            qm.agency.week = sum(qm.agency),
            # Note: slightly different than in tweet-level data.
            # This variable is constructed from Twitter's metrics and 
            # therefore also includes, for example, non-English tweets
            reply.count.week = sum(reply_count)) %>%
  mutate(week = round(as.POSIXct(week), "day"),
         mention.ratio = ifelse(!(information.week), 0, (mention.week/information.week)),
         qm.agency.ratio = ifelse(!(information.week), 0, (qm.agency.week/information.week)),
         reply.count.ratio = ifelse(!(information.week), 0, (reply.count.week/information.week))) %>%
  select(-information.week)

# Week
response.panel <- sentiment.data %>%
  group_by(agencyname,
           week = cut(day, "week"),
           .drop = FALSE) %>%
  summarise(offset.week = n(),
            response.week = sum(response.num),
            attachment.week = sum(attachement),
            qm.comment.week = sum(qm.comment),
            short.week = sum(short))  %>%
  mutate(week = round(as.POSIXct(week), "day"),
         attachment.ratio = ifelse(!(offset.week), 0, (attachment.week/offset.week)),
         qm.comment.ratio = ifelse(!(offset.week), 0, (qm.comment.week/offset.week)),
         short.ratio = ifelse(!(offset.week), 0, (short.week/offset.week))) %>%
  left_join(twitter.week, by = c("agencyname", 'week')) %>% 
  left_join(agency.week, by = c("agencyname", 'week')) %>% 
  mutate(time.on.Twitter = difftime(strptime(week, format = "%Y-%m-%d"),
                             strptime(join.day, format = "%Y-%m-%d"),
                             units = c("weeks")),
         twitter.valence = twitter.praise - twitter.criticism) %>%
  filter(time.on.Twitter >= 0)

# ======================================================
#          save data
# ======================================================

save(response.tweet, file = "response_tweet.Rda")
save(response_panel, file = "response_panel.Rda")



