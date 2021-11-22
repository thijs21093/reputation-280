library(dplyr)
library(ggplot2)
library(ggthemes)

# Load data
hearing <- read.csv("./Data/hearing/hearing_reduced.csv",
                    sep = ";")

# Data manipulation
hearing.event <- hearing %>% 
  filter(type == "FA" | type == "both") %>%
  mutate(date = as.Date(date,  "%d-%m-%Y"))

# By day
hearing.day <- hearing.event %>%
  group_by(agencyname,
           day = cut(date, "day"),
           .drop = FALSE) %>%
  summarise(hearing = n(),
            .groups = "keep") %>%
  mutate(day = as.Date(day)) %>%
  filter(day >= "2015-07-01" & # Start date
           day <= "2021-06-30") %>% # End date
 filter(agencyname != "BBI",
        agencyname != "Cepol",
        agencyname != "Clean Sky",
        agencyname != "EASO",
        agencyname != "EDA",
        agencyname != "EDPS",
        agencyname != "EIT",
        agencyname != "EPPO",
        agencyname != "ETF",
        agencyname != "Europol",
        agencyname != "EU-LISA",
        agencyname != "EUISS",
        agencyname != "Eurojust",
        agencyname != "F4E",
        agencyname != "GSA",
        agencyname != "IMI",
        agencyname != "SESAR") # Removing non-regulatory agencies

write.csv(hearing.day, "hearing_day.csv")
