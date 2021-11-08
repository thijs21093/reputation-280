# Library
library(dplyr)

# Clear environemnt
rm(list = ls())

# Data
load(file="./Data/from_tweets") # Data

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
## information.month <- information %>%
##   group_by(agencyname,
##            month = cut(created_at, "month"),
##           .drop = FALSE) %>%
##   summarise(information = n(),
##             .groups = "keep") %>%
##   mutate(first_match = min(row_number()[information != 0])) %>% # Set count to NA when an agency has no Twitter
##   filter(row_number() >= first_match) %>%
##  mutate(month = as.POSIXct(month)) %>%
##   select(-c(first_match)) # Remove vars

# TO DO: Fix filtering
# TO DO: Check filtering 'audience'

# Plotting information
information.month %>% ggplot(aes(x = month, y = information)) +
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

# TO DO: Add replies to users