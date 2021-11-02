#libraries
library(tidyverse)
library(anomalize)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Load data
load("./data/audiencecounts")

# Create dataframe
for (i in seq(audiencecounts))
  assign(paste0("df", i), audiencecounts[[i]]) # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- do.call(rbind, mget(names(dfs)[dfs])) # Bind dataframes

df.month <- df %>%
  mutate(start = as.POSIXct(start),
         end = as.POSIXct(end)) %>% # Adjust time variables
  filter(start >= "2015-07-01" & # Start date
         start <= "2021-06-30") %>% # End date
  group_by(agencyname, month = cut(start, "month"),  .drop = FALSE) %>%
  summarise(count = sum(tweet_count), .groups = "keep") %>%
  mutate(first_match = min(row_number()[count != 0])) %>% # Set count to NA when an agency has no Twitter
  filter(row_number() >= first_match) %>%
  mutate(month = as.POSIXct(month)) %>%
 select(-c(first_match)) # Remove vars

# TO DO: check warning

# Plot time 
df.month %>% ggplot(aes(x = month, y = count)) +
  facet_wrap(~agencyname,
             ncol = 6,
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

# Convert df to a tibble
df.t <- as_tibble(df.month) %>%
  group_by(agencyname) # Create grouped tibble


# ======================================================
#           STL
# ======================================================

df.anomalized <- df.t %>%
  time_decompose(count, merge = TRUE, method = "STL") %>% # STL is default
  anomalize(remainder, method = "gesd") %>%
  anomalize::time_recompose()

# TO DO: check warnings

# Plot
df.anomalized %>%
  plot_anomalies(ncol = 5, alpha_dots = 0.25)

# Extracting the anomalous data points
anomaly <- df.anomalized %>%
  mutate(threat = case_when(
    anomaly == "Yes" & remainder > 0 ~ "yes",
    TRUE ~ "no"))

# Distribution
anomaly %>%
  group_by(threat) %>%
  dplyr::summarise(count = n()) %>%
  mutate(prop = count / sum(count)) # Overview of distribution across sentiments

# ======================================================
#           Twitter decomposition
# ======================================================
df.anomalized.twitter <- df.t %>%
  time_decompose(count, merge = TRUE, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>% # Same as Erlich et al.
  anomalize::time_recompose()

# TO DO: check warnings

# Plot
df.anomalized.twitter %>%
  plot_anomalies(ncol = 5, alpha_dots = 0.25)

# Extracting the anomalous data points
anomaly.twitter <- df.anomalized.twitter %>%
  mutate(threat = case_when(
    anomaly == "Yes" & remainder > 0 ~ "yes",
    TRUE ~ "no"))

# Distribution
anomaly.twitter %>%
  group_by(threat) %>%
  dplyr::summarise(count = n()) %>%
  mutate(prop = count / sum(count)) # Overview of distribution across sentiments
