# Library
library(dplyr)
library(ggplot2)
library(ggthemes)
library(vader)
library(data.table)
library(tidyr)
library(anomalize)
library(zoo)
library(qdapRegex)
library(stringr)
library(lubridate)

load(file="./Data/from_tweets") # Data
load(file="./Data/to_tweets") # Data

# ======================================================
#           Information
# ======================================================
from.agency <- tibble.from %>%
  mutate(created_at = as.Date(created_at)) %>%
  filter(referenced_type != "retweeted" & # Remove retweets
         referenced_type != "quoted") %>% # Remove quoted tweets
  distinct(tweet_id, .keep_all = TRUE)  %>% # remove duplicates
  filter(created_at >= "2015-07-01" & # Start date
         created_at <= "2021-06-30") # End date 

from.agency %>%
  group_by(referenced_type) %>%
  dplyr::summarise(count = n()) # Count

update <- from.agency %>%
  filter(referenced_type == "no reference" &
          is.na(in_reply_to_user_id)) # Initial updates
update %>% nrow() # Number of replies to self

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

reply.to.status %>% nrow() # Count of replies to users

information <- bind_rows(agency.to.self,
                         update,
                         reply.to.user) # Bind updates and replies to self
information %>% nrow() # Count number of agency updates


# Information per day
information.day<- information %>%
   group_by(agencyname,
            day = cut(created_at, "day"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  group_by(agencyname) %>%
   mutate(first_match = min(row_number()[information != 0]), # Get row number of first non-zero for each agency
   joined = case_when( 
    agencyname == "FRONTEX" & row_number() < first_match | # Agencies that joined Twitter after 01-07-2015
    agencyname == "ACER" & row_number() < first_match |
    agencyname == "ERA" & row_number() < first_match ~ "no",
     TRUE ~ "yes"), # Joined = 'yes' for all other agencies
   information = case_when(
     joined == "no" ~ NA_real_, # Set count to NA when an agency has no Twitter
     joined == "yes" ~ as.numeric(as.character(information)))) %>%
  mutate(day = format(round(as.POSIXct(day), "day"))) %>%
   select(-c(first_match)) # Remove var

# ======================================================
#           Responsiveness
# ======================================================

to.agency <- tibble.to %>%
  mutate(created_at = as.Date(created_at)) %>%
  filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
           in_reply_to_user_id != author_id & # Remove tweets to self
           lang == "en") %>% # Remove non-English tweets
  distinct(tweet_id, .keep_all = TRUE)  %>% # remove duplicates
  filter(created_at >= "2015-07-01" & # Start date
           created_at <= "2021-06-30") # End date 

# Create lists of tweets ids referenced by agencies
response.filter <- from.agency %>%
  filter(!is.na(referenced_id)) %>%
  dplyr::select(referenced_id) %>% 
  as.list()

# TO DO: convert '&lt;', '&amp;', etc. to correct format

response  <- to.agency %>%
  dplyr::mutate(response = case_when(
    tweet_id %in% response.filter[["referenced_id"]] ~ "response", # Code as 'response' when tweet is AT LEAST ONCE responded to
    !tweet_id %in% response.filter[["referenced_id"]] ~ "no.response")) %>% # Code as 'no.response' if not
  mutate(text.url = str_remove_all(text, "@[[:alnum:]_]{2,}"),# Remove user
         text.url = rm_url(text.url, pattern = pastex("@rm_twitter_url", "@rm_url", "rm_white_lead")),  # Remove url
         question.mark = ifelse(grepl("\\?", text.url), 1, 0), # Does text contain question mark?
  length = str_length(text.url), # Length of text without urls/mentions (@[user])
  time = difftime(strptime(created_at, format = "%Y-%m-%d"),
                   strptime("2015-07-01", format = "%Y-%m-%d"),
                  units = c("days")) %>% # Number of days passed since start
    round(0) %>% # Ignore differences because of summer/winter time
    as.numeric()) # Numeric

response %>%
  group_by(response) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

# ======================================================
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
  
# Load other data
audience <- read.csv("audience/audience_day.csv") %>%
  mutate(day = format(round(as.POSIXct(day), "day"))) %>%
  select(-X) 
hearing <- read.csv("hearing/hearing_day.csv") %>%
  mutate(day = format(round(as.POSIXct(day), "day"))) %>%
  select(-X)
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



