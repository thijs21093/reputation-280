# Library
library(dplyr)
library(ggplot2)
library(ggthemes)
library(vader)
library(data.table)
library(tidyr)
library(anomalize)
library(zoo)

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

# Information per month
information.month <- information %>%
   group_by(agencyname,
            month = cut(created_at, "month"),
           .drop = FALSE) %>%
  summarise(information = n()) %>%
  group_by(agencyname) %>%
   mutate(first_match = min(row_number()[information != 0]), # Get row number of first non-zero for each agency
   joined = case_when(
     row_number() >= first_match ~ "yes", # Assumes that first 0 is because of not being on Twitter
     row_number() < first_match ~ "no"), # Manually checked: assumption is true
   information = case_when(
     joined == "no" ~ NA_real_, # Set count to NA when an agency has no Twitter
     joined == "yes" ~ as.numeric(as.character(information)))) %>%
  mutate(month = as.POSIXct(month)) %>%
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

response  <- to.agency %>%
  dplyr::mutate(response = case_when(
    tweet_id %in% response.filter[["referenced_id"]] ~ "response", # Code as 'response' when tweet is AT LEAST ONCE responded to
    !tweet_id %in% response.filter[["referenced_id"]] ~ "no.response")) # Code as 'no.response' if not

response.month <- response %>%
  group_by(agencyname,
           month = cut(created_at, "month"), # Per month
           cat = response, # Count tweets with responses and without responses
           .drop = FALSE)  %>%
  summarise(repsponse = n(),
            .groups = "keep") %>%
  data.table() %>%
  pivot_wider(names_from = "cat", # Responses and no responses in seperate colums (wide format)
              values_from = "repsponse",
              values_fill = 0) %>% # Fill missing columns with 0
  dplyr::select(-`NA`) %>%
  mutate(discussion = response + no.response, # Offset
         month = as.POSIXct(month)) 


# ======================================================
#           Consequences
# ======================================================
sentiment <- vader_df(to.agency$text,
                            incl_nt = T,
                            neu_set = T,
                            rm_qm = T) # Calculate valence of tweets

consequences <- merge(sentiment,
                      to.agency,
                      by.y = "text",
                      by.x = "text") %>% # Merge scores with data
  distinct() %>% # TO DO: Why are duplicates created?
  mutate(sentiment = case_when(
    compound <= -0.05 ~ "negative",
    compound > -0.05 & compound < 0.05 ~ "neutral",
    compound >= 0.05 ~ "positive"))

# Distribution
consequences %>%
  group_by(sentiment) %>%
  dplyr::summarise(count = n())

consequences.month <- consequences %>%
  dplyr::group_by(agencyname, month = cut(created_at, "month"),  .drop = FALSE)  %>% 
  dplyr::count(sentiment) %>%
  pivot_wider(names_from = "sentiment",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::select(-`NA`) %>%
  mutate(month = as.POSIXct(month))

# ======================================================
#           Joining dataframes
# ======================================================

# Join dataframes
twitter.month <- information.month %>%
  full_join(response.month,  by = c("agencyname", "month")) %>%
  full_join(consequences.month,  by = c("agencyname", "month"))
  
# Load other data
audience <- read.csv("audience/audience.csv") %>%
  mutate(month = as.POSIXct(month)) %>% # TO DO: change name of column from 'count' to 'audience_count'
  select(-X) 
hearing <- read.csv("hearing/hearing.csv") %>%
  mutate(month = as.POSIXct(month)) %>%
  select(-X)
media <- read.csv("media/media.csv") %>%
  mutate(month = as.POSIXct(month)) %>%
  select(-X)

total.month <- twitter.month %>%
  full_join(audience,  by = c("agencyname", "month")) %>%
  full_join(hearing,  by = c("agencyname", "month")) %>%
  full_join(media,  by = c("agencyname" = "acronym", "month")) %>%
  mutate(hearing = replace_na(hearing, 0)) %>%  # Set 'hearing' to 0 when agency has never been subject of hearing
        mutate_at(.vars = c("response",
                            "no.response",
                            "discussion",
                            "positive",
                            "neutral",
                            "negative",
                            "audience"),
                  funs(ifelse(joined == "no", NA, .)))


# ======================================================
#           Anomaly detection: Audience
# ======================================================

# Convert df to a tibble
df.t <- as_tibble(total.month) %>%
  group_by(agencyname) # Create grouped tibble

df.anomalized.twitter <- df.t %>%
  drop_na() %>%
  time_decompose(audience, merge = TRUE, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>% # Same as Erlich et al.
  anomalize::time_recompose()

# TO DO: check warnings

# Plot
df.anomalized.twitter %>%
  plot_anomalies(ncol = 5, alpha_dots = 0.25)

# Extracting the anomalous data points
anomaly.twitter <- df.anomalized.twitter %>%
  mutate(audience_threat = case_when(
    anomaly == "Yes" & remainder > 0 ~ "yes",
    TRUE ~ "no"))

# Distribution
anomaly.twitter %>%
  group_by(audience_threat) %>%
  dplyr::summarise(count = n()) %>%
  mutate(prop = count / sum(count)) # Overview of distribution across sentiments

# Bind dataframes
all.vars <- total.month %>%
  full_join(anomaly.twitter %>% select(c(agencyname, month, audience_threat)),
            by = c("agencyname", "month"))

# ======================================================
#           Rolling sum
# ======================================================
data <- all.vars %>%
  arrange(month) %>% # Arrange by month
  group_by(agencyname) %>%
dplyr::mutate(criticism.12 = rollapplyr(negative, list(seq(-12, -1)), sum, fill = NA, partial = FALSE, align = "right"),
              praise.12 = rollapplyr(positive, list(seq(-12, -1)), sum, fill = NA, partial = FALSE, align = "right"),
              neutral.12 = rollapplyr(neutral, list(seq(-12, -1)), sum, fill = NA, partial = FALSE, align = "right"),
              valence.12 = ifelse(!(praise.12 + neutral.12 + criticism.12), 0, (praise.12 - criticism.12)/ (praise.12 + neutral.12 + criticism.12)),
                criticism.3 = rollapplyr(negative, list(seq(-3, -1)), sum, fill = NA, partial = FALSE, align = "right"),
                praise.3 = rollapplyr(positive, list(seq(-3, -1)), sum, fill = NA, partial = FALSE, align = "right"),
                neutral.3 = rollapplyr(neutral, list(seq(-3, -1)), sum, fill = NA, partial = FALSE, align = "right"),
                valence.3 = ifelse(!(praise.3 + neutral.3 + criticism.3), 0, (praise.3 - criticism.3)/ (praise.3 + neutral.3 + criticism.3)),
                  media_count.3 = rollapplyr(media_count, list(seq(-3, -1)), sum, fill = NA, partial = FALSE, align = "right"),
                  media_count.12 = rollapplyr(media_count, list(seq(-12, -1)), sum, fill = NA, partial = FALSE, align = "right"))

# Export
write.csv(data, "data.csv")

# ======================================================
#           Plots
# ======================================================

# Information
data %>% ggplot(aes(x = as.Date(month), y = information)) +
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
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
  xlab("Time") + ylab("Count") + labs(title = "Information (07/15 - 06/21)")

# Responsiveness
data %>% ggplot(aes(x = as.Date(month), y = ifelse(!discussion, 0, (response/discussion)))) +
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
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

# Responsiveness
data %>% ggplot(aes(x = as.Date(month), y = valence.3)) + # 3 Months rolling sum
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
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
  xlab("Time") + ylab("Count") + labs(title = "Valence of replies, 3 months rolling sum (07/15 - 06/21)")

# Audience
data %>% ggplot(aes(x = as.Date(month), y = audience_count)) +
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 0.5) +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_few() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 10)) +
  xlab("Time") + ylab("Count") + labs(title = "Audience tweets (07/15 - 06/21)")

# Media
data %>% ggplot(aes(x = as.Date(month), y = media_count.3)) + # 3 months rolling sum
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
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
  xlab("Time") + ylab("Count") + labs(title = "Appearances in the media (07/15 - 06/21)")

# Hearing
data %>% ggplot(aes(x = as.Date(month), y = hearing)) +
  facet_wrap(~agencyname,
             ncol = 5,
             scales = "free") +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_few() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 10)) +
  xlab("Time") + ylab("Count") + labs(title = "Non-routine hearings (07/15 - 04/19)")
