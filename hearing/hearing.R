library(dplyr)
library(ggplot2)
library(ggthemes)

# Load data

# Data manipulation
hearing.event <- hearing %>% 
  filter(type == "FA" | type == "both") %>%
  mutate(date = as.Date(date,  "%d-%m-%Y"))

# By month
hearing.month <- hearing.event %>%
  group_by(agency,
           month = cut(date, "month"),
           .drop = FALSE) %>%
  summarise(hearing = n(),
            .groups = "keep") %>%
  mutate(month = as.Date(month)) %>%
  filter(month >= "2015-07-01" & # Start date
           month <= "2021-06-30") %>% # End date
 filter(agency != "BBI",
        agency != "Cepol",
        agency != "Clean Sky",
        agency != "EASO",
        agency != "EDA",
        agency != "EDPS",
        agency != "EIT",
        agency != "EPPO",
        agency != "ETF",
        agency != "Europol",
        agency != "EU-LISA",
        agency != "EUISS",
        agency != "Eurojust",
        agency != "F4E",
        agency != "GSA",
        agency != "IMI",
        agency != "SESAR") # Removing non-regulatory agencies

# Plot 
hearing.month %>% ggplot(aes(x = month, y = hearing)) +
  facet_wrap(~agency,
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
