# Libraries
library(tidyverse)
library(anomalize)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Clear environemnt
rm(list = ls())

# Load data
load("./Data/audiencecounts_updated")

# Create dataframe
for (i in seq(audiencecounts))
  assign(paste0("df", i), audiencecounts[[i]]) # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- do.call(rbind, mget(names(dfs)[dfs])) # Bind dataframes

audience.month <- df %>%
  mutate(start = as.POSIXct(start),
         end = as.POSIXct(end)) %>% # Adjust time variables
  filter(start >= "2015-07-01" & # Start date
         start <= "2021-06-30") %>% # End date
  group_by(agencyname,
           month = cut(start, "month"),  
           .drop = FALSE) %>%
  summarise(audience_count = sum(tweet_count),
            .groups = "keep") %>%
  mutate(month = as.POSIXct(month))  # Remove vars

write.csv(audience.month, "audience.csv")


