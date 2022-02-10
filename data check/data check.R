# Library
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyquant)

load(file="./Data/from_tweets") # Data
load(file="./Data/to_tweets") # Data

load("[PATH]/to_data_sentiment.RData") # Data collected via paid subscription
load("[PATH]/from_data_en.RData") # Data collected via paid subscription

# No. of observations per agency
# ======================================================
from.count <- tibble.from %>%
  group_by(agencyname) %>%
  count()

view(from.count)

to.count <- tibble.to %>%
  group_by(agencyname) %>%
  count()

view(to.count)

# Check if data is complete
# ======================================================

# To agencies
complete.to.filter <- tibble.to %>%
  dplyr::select(tweet_id) %>% 
  mutate(tweet_id = tweet_id) %>%
  as.list() # Create list with IDs

complete.to  <- to_data_sentiment %>%
  filter(in_reply_to_screen_name == "eu_acer" |
         in_reply_to_screen_name == "BERECeuropaeu" |
         in_reply_to_screen_name == "Cedefop" |
         in_reply_to_screen_name == "CPVOTweets" |
         in_reply_to_screen_name == "EASA" |
         in_reply_to_screen_name == "EBA_News" |
         in_reply_to_screen_name == "ECDC_EU" |
         in_reply_to_screen_name == "EU_ECHA" |
         in_reply_to_screen_name == "EUEnvironment" |
         in_reply_to_screen_name == "EFCA_EU" |
         in_reply_to_screen_name == "EFSA_EU" |
         in_reply_to_screen_name == "eurogender" |
         in_reply_to_screen_name == "eiopa_europa_eu" |
         in_reply_to_screen_name == "EMA_News" |
         in_reply_to_screen_name == "EMCDDA" |
         in_reply_to_screen_name == "EMSA_LISBON" |
         in_reply_to_screen_name == "enisa_eu"  |
         in_reply_to_screen_name == "ERA_railways" |
         in_reply_to_screen_name == "ESMAComms" |
         in_reply_to_screen_name == "EU_OSHA" |
         in_reply_to_screen_name == "EU_IPO" |
         in_reply_to_screen_name == "eurofound" |
         in_reply_to_screen_name == "EURightsAgency" |
         in_reply_to_screen_name == "Frontex" |
         in_reply_to_screen_name == "EU_SRB") %>%
  dplyr::mutate(complete = case_when(
    id_str %in% complete.to.filter[["tweet_id"]] ~ "TRUE",
    !id_str %in% complete.to.filter[["tweet_id"]] ~ "FALSE"))

complete.to %>%
  group_by(complete) %>%
  count()

# From agencies
complete.from.filter <- tibble.from %>%
  dplyr::select(tweet_id) %>% 
  mutate(tweet_id = tweet_id) %>%
  as.list() # Create list with IDs

complete.from  <- from_data_en %>%
  filter(user_screen_name == "eu_acer" |
           user_screen_name == "BERECeuropaeu" |
           user_screen_name == "Cedefop" |
           user_screen_name == "CPVOTweets" |
           user_screen_name == "EASA" |
           user_screen_name == "EBA_News" |
           user_screen_name == "ECDC_EU" |
           user_screen_name == "EU_ECHA" |
           user_screen_name == "EUEnvironment" |
           user_screen_name == "EFCA_EU" |
           user_screen_name == "EFSA_EU" |
           user_screen_name == "eurogender" |
           user_screen_name == "eiopa_europa_eu" |
           user_screen_name == "EMA_News" |
           user_screen_name == "EMCDDA" |
           user_screen_name == "EMSA_LISBON" |
           user_screen_name == "enisa_eu"  |
           user_screen_name == "ERA_railways" |
           user_screen_name == "ESMAComms" |
           user_screen_name == "EU_OSHA" |
           user_screen_name == "EU_IPO" |
           user_screen_name == "eurofound" |
           user_screen_name == "EURightsAgency" |
           user_screen_name == "Frontex" |
           user_screen_name == "EU_SRB") %>%
  dplyr::mutate(complete = case_when(
    id_str %in% complete.from.filter[["tweet_id"]] ~ "TRUE",
    !id_str %in% complete.from.filter[["tweet_id"]] ~ "FALSE"))

complete.from %>%
  group_by(complete) %>%
  count()

# # No. of observations per agency over time
# ======================================================

# Tweets from agencies
from.freq <- tibble.from %>% 
  mutate(created_at = as.Date(created_at))  %>% 
  count(created_at, agencyname, name = "freq")

from.freq %>% ggplot(mapping = 
                       aes(x = created_at, y = freq,
                      color = agencyname, facet)) + 
  facet_wrap(~ agencyname,   scales = "free_y") +
  geom_line() +
  geom_ma(ma_fun = SMA, n = 30, color = "black", linetype = "solid") +  # Plot 30-day SMA
ggtitle("No. of tweets from agencies") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme(legend.position = 'none',
        axis.title = element_blank()) 

# Tweets to agencies
to.freq <- tibble.to %>% 
  mutate(created_at = as.Date(created_at))  %>% 
  count(created_at, agencyname, name = "freq")

to.freq %>% ggplot(mapping = 
                       aes(x = created_at, y = freq,
                           color = agencyname, facet)) + 
  facet_wrap(~ agencyname, scales = "free_y") +
  geom_line() + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  ggtitle("No. of tweets to agencies") +
  theme(legend.position = 'none',
        axis.title = element_blank())

# Responses over time
# ======================================================

# Free data
response.free <- tibble.from %>%
  filter(referenced_type == "no reference" &
           in_reply_to_user_id != author_id) %>%
  mutate(created_at = as.Date(created_at))  %>% 
  count(created_at, agencyname, name = "freq")

response.free %>% ggplot(mapping = 
                     aes(x = created_at, y = freq,
                         color = agencyname, facet)) + 
  facet_wrap(~ agencyname,   scales = "free_y") +
  geom_line() + 
  geom_ma(ma_fun = SMA, n = 30, color = "black", linetype = "solid") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        title = element_text("No. of responses (free)"))

# Recalculating response
response.filter.free <- tibble.from %>%
  dplyr::select(referenced_id) %>% 
  drop_na %>%
  as.list() # 

response.free  <- tibble.to %>%
  dplyr::mutate(response = case_when(
    tweet_id %in% response.filter.free[["referenced_id"]] ~ "response", # Code as 'response' when agency tweet is AT LEAST ONCE responded to
    !tweet_id %in% response.filter.free[["referenced_id"]] ~ "no.response"), # Code as 'no.response' if not
    created_at = as.Date(created_at))  %>% 
  filter(response == "response" &
         lang == "en"  &
         in_reply_to_user_id != author_id,
         !is.na(referenced_id))  %>%
  count(created_at, agencyname, name = "freq")

response.free %>% ggplot(mapping = 
                           aes(x = created_at, y = freq,
                               color = agencyname, facet)) + 
  facet_wrap(~ agencyname,   scales = "free_y") +
  geom_line() + 
  ggtitle("No. of responses recalculated (free)") +
  geom_ma(ma_fun = SMA, n = 30, color = "black", linetype = "solid") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme(legend.position = 'none',
        axis.title = element_blank())
