# Library
library(dplyr)
library(ggplot2)
library(ggthemes)
library(vader)
library(data.table)
library(tidyr)
library(zoo)
library(qdapRegex)
library(stringr)
library(lubridate)
library(sjmisc)

load(file="./data (not public)/from_tweets/from_tweets") # Data
load(file="./data (not public)/to_tweets/to_tweets") # Data

# ======================================================
#           Information
# ======================================================

# TO DO: double check code in this section

from.agency <- tibble.from %>%
  mutate(created_at = as.Date(created_at),
         ) %>%
  filter(referenced_type != "retweeted" & # Remove retweets
         referenced_type != "quoted",
         lang == "en") %>% # Remove quoted tweets
  distinct(tweet_id, .keep_all = TRUE) # remove duplicates

from.agency %>%
  group_by(referenced_type) %>%
  dplyr::summarise(count = n()) # Count

update <- from.agency %>%
  filter(referenced_type == "no reference" &
          is.na(in_reply_to_user_id)) # Initial updates
update %>% nrow() # Number initial updates

agency.to.self <- from.agency %>%
  dplyr::filter(in_reply_to_user_id == author_id) # Tweets to self
agency.to.self %>% nrow() # Number of replies to self

reply.to.user <- from.agency %>%
  filter(referenced_type == "no reference" &
           in_reply_to_user_id != author_id) # Replies to users (but not replies to users' statuses)
reply.to.user %>% nrow() # Number of replies to users

reply.to.status <- from.agency %>%
  filter(referenced_type == "replied_to" &
        in_reply_to_user_id != author_id)

reply.to.status %>% nrow() # Count of replies to user statuses

# To do: include reply to status?
# It might be that agencies can comments on those tweets.

information <- bind_rows(agency.to.self,
                         update) # Bind updates and replies to self

information %>% nrow() # Count number of tweets in data


# Information per week
information.week <- information %>%
   group_by(agencyname,
            week = cut(created_at, "week"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  mutate(week = format(round(as.POSIXct(week), "day")))

# ======================================================
#           Find joining date
# ======================================================

joining.date <- information.week %>% 
  group_by(agencyname = factor(agencyname)) %>% 
  filter(any(information == 0)) %>% 
  filter(information != 0) %>% 
  arrange(week) %>% 
  slice(1)  %>%
  ungroup() %>% 
  complete(agencyname)
  
# Note some agencies joined before 1 January, 2010.

# ======================================================
#           Responsiveness
# ======================================================

#           Creating variables
# ======================================================

to.agency <- tibble.to %>%
  mutate(created_at = as.Date(created_at),
        text = textutils::HTMLdecode(text)) %>% # Decode html
  unnest(cols = attachments, # Unnest attachment
                  keep_empty = TRUE) %>%
  filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
           in_reply_to_user_id != author_id & # Remove tweets to self
           lang == "en") %>% # Remove non-English tweets
  distinct(tweet_id, .keep_all = TRUE) # remove duplicates

# Create lists of tweets ids referenced by agencies
response.filter <- from.agency %>%
  filter(!is.na(referenced_id)) %>%
  dplyr::select(referenced_id) %>% 
  as.list()

to.agency2  <- to.agency %>% 

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
  attachement = case_when(media_keys != "NULL" ~ 1, # TO do: Check what's included in media_keys
                          TRUE ~ 0),
  
  # Real time
  real.time = difftime(strptime(created_at, format = "%Y-%m-%d"),
                   strptime("2010-01-04", format = "%Y-%m-%d"),
                  units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric(), # Numeric
  
  # Year
  year =  format(as.POSIXct(day), format= "%Y"), 
  
  # weekend versus weekday
         weekday = weekdays(as.Date(day)), 
         weekend = case_when(
           weekday == "zaterdag" ~ "weekend", # Saturday
           weekday == "zondag" ~ "weekend",   # Sunday
           TRUE ~ "weekday"))

#           Number of tweets in conversation
# ======================================================

conversation <- response %>% group_by(conversation_id) %>%
  summarise(conversations = length(conversation_id))

response <- to.agency2 %>%
  full_join(conversation, by = "conversation_id")

#           Question mark/mention in agency tweet
# ======================================================

mentions <- information %>%

  # Select variables
  select(c(entities, text, tweet_id, agencyhandle)) %>% 
  
  # Unnnest entities and select variables
  unnest(cols = c(entities)) %>%
  select(c(mentions, text, tweet_id, agencyhandle)) %>%
  
  # Unnnest mentions and keep empty rows
  unnest(cols = c(mentions)) %>%
  
  # Filter out mentions of self
  mutate(agencyhandle2 = str_remove_all(agencyhandle, "@")) %>%
  filter(agencyhandle2 != username) %>%
  
  # Create list with tweet_ids of tweets that include a mention
  dplyr::select(tweet_id) %>% 
  as.list()
  

engaging <- information %>%
  mutate(
    
    # Does contain a mention?
    mention = case_when(
      tweet_id %in% mentions[["tweet_id"]] ~ 1, # Code as 1 if tweet mentions AT LEAST one user
      !tweet_id %in% mentions[["tweet_id"]] ~ 0), # Code as 0 if not
    
    # Check if agency tweet contains handle of a user other than the agency sending the tweet 
    text.url = str_remove_all(text, "@[[:alnum:]_]{2,}"),# Remove user
    text.url = rm_url(text.url, pattern = pastex("@rm_twitter_url", "@rm_url", "rm_white_lead")),  # Remove url  
    
  # Question mark in agency tweet
  qm.agency = ifelse(grepl("\\?", text.url), 1, 0)) %>%
  select(c(mention, qm.agency, tweet_id))

# Add new variable to response df
response2 <- response %>%
  left_join(engaging, # Left join, b/c we only want tweets TO agencies
            by = c("referenced_id" = "tweet_id")) # Link between tweets from agencies and tweet to agencies

response2 %>%
  select(c(mention, qm.agency)) %>% 
  descr # 10% missing

#           Quality check response variable
# ======================================================
response %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

na.check <- response2 %>%
  filter(response == "response" &
           is.na(qm.agency))

#           Consequences
# ======================================================
sentiment <- vader_df(response$text,
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

# Consequences
consequences.day <- tweet.data %>%
  dplyr::group_by(agencyname, day = cut(created_at, "day"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))

# Remove columns
tweet.data <- tweet.data %>%
  dplyr::select(where(is.numeric), where(is.character), where(is.Date)) # remove non-numeric & non-character rows

# ======================================================
#           Joining dataframes
# ======================================================

# Join dataframes
twitter.day <- information.day %>%
  full_join(consequences.day,  by = c("agencyname", "day")) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))
  
# Load media data
media <- read.csv("media/media_day.csv") %>%
  mutate(day = format(round(as.POSIXct(day), "day"))) %>%
  select(-X)

# To do: save as .Rdata instead of csv

total.day <- twitter.day %>%
  full_join(audience,  by = c("agencyname", "day")) %>%
  full_join(hearing,  by = c("agencyname", "day")) %>%
  full_join(media,  by = c("agencyname" = "acronym", "day")) %>%
  mutate(hearing = tidyr::replace_na(hearing, 0), # Set 'hearing' to 0 when agency has never been subject of hearing
         hearing = case_when(day > "2019-05-01" ~ NA_real_, 
           TRUE ~ as.numeric(hearing))) %>%  # Set all values for variable 'hearing' before 01-05-2019 to NA
        mutate_at(.vars = c("positive",
                            "neutral",
                            "negative",
                            "audience_count"),
                  funs(ifelse(joined == "no", NA, .))) %>% # Twitter variable to NA when agency was not on Twitter
  mutate(day = format(round(as.POSIXct(day), "day")))

# ======================================================
#           Anomaly detection: Audience
# ======================================================

# Convert df to a tibble
df.t <- as_tibble(total.day) %>%
  group_by(agencyname) %>% # Create grouped tibble
  mutate(day = as.Date(day)) %>%
  select(c(day, agencyname, audience_count))

df.anomalized.twitter <- df.t %>%
  drop_na() %>%
  time_decompose(audience_count, merge = TRUE, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>% # Same as Erlich et al.
  anomalize::time_recompose()
warnings()

# TO DO: check warnings

# Plot
df.anomalized.twitter %>%
  plot_anomalies(ncol = 5, alpha_dots = 0.25)

# Extracting the anomalous data points
anomaly.twitter <- df.anomalized.twitter %>%
  mutate(audience_threat = case_when(
    anomaly == "Yes" & remainder > 0 ~ 1,
    TRUE ~ 0),
    audience_silence = case_when(
      anomaly == "Yes" & remainder < 0 ~ 1,
      TRUE ~ 0)) %>%
  select(c(day, agencyname, audience_threat, audience_silence)) %>%
  mutate(day = format(round(as.POSIXct(day), "day")))

# Distribution
anomaly.twitter %>%
  group_by(audience_threat, audience_silence) %>%
  dplyr::summarise(count = n()) %>%
  mutate(prop = count / sum(count)) # Overview of distribution across sentiments

# Bind dataframes
all.vars <- total.day %>%
  full_join(anomaly.twitter,
            by = c("agencyname", "day"))

# ======================================================
#           Rolling sum/lags
# ======================================================
data.day <- all.vars %>%
  arrange(day) %>% # Arrange by day
  group_by(agencyname) %>%
dplyr::mutate(criticism.3m = rollapplyr(negative,
                                       list(seq(-90, -1)),
                                       sum, fill = NA,
                                       align = "right"),
              praise.3m = rollapplyr(positive,
                                    list(seq(-90, -1)),
                                    sum, fill = NA,
                                    align = "right"),
              neutral.3m = rollapplyr(neutral,
                                     list(seq(-90, -1)),
                                     sum, fill = NA,
                                     align = "right"),
              valence.3m = ifelse(!(praise.3m + neutral.3m + criticism.3m),
                                 0,
                                 (praise.3m - criticism.3m) / (praise.3m + neutral.3m + criticism.3m)),
              media_count.3m = rollapplyr(media,
                                         list(seq(-90, -1)),
                                         sum,
                                         fill = NA,
                                         align = "right"),
              hearing.3m = rollapplyr(hearing,
                                         list(seq(-90, -1)),
                                         sum,
                                         fill = NA,
                                         align = "right"),
              information.1w = rollapplyr(information,
                                          list(seq(-7, -1)),
                                          sum,
                                          fill = NA,
                                          align = "right"),
              audience_count.1d = lag(audience_count, k = -1),
              audience_threat.1d = lag(audience_threat, k = -1),
              audience_silence.1d = lag(audience_silence, k = -1))

# ======================================================
#           Plot
# ======================================================

# Responsiveness
tweet.data %>% 
group_by(response, created_at) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  mutate(month = lubridate::floor_date(created_at, unit = "month")) %>%
  group_by(month) %>%            # group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  mutate(ratio = response/(response + no.response)) %>%
  ggplot(aes(x = as.Date(month), y = ratio)) +
  geom_smooth(method = "loess",
              se = FALSE,
              size = 0.5) +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y") +
  theme_few() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 10)) +
  xlab("Time") + ylab("Count") + labs(title = "Responsiveness (07/15 - 06/21)")

# ======================================================
#           Merging
# ======================================================
tweet.data <- tweet.data %>%
  mutate(day = format(round(as.POSIXct(created_at), "day")), .keep = "unused")

data.merge <- tweet.data %>%
  left_join(data.day,
            by = c("agencyname", "day"))

save(data.merge, file = "data_day.Rda")



