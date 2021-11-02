library(tm.plugin.factiva)
library(tm)
library(purrr)
library(tidytext)
library(dplyr)
library(stringr)

# All 
files.list <- list.files(full.names = TRUE) # List of paths
html <- str_extract(files.list, "\\w*\\.html") # List of htmls
files <- html[!is.na(html)] # Creates a list of all valid files in folder
path <- str_c(getwd(), "/", files) # Construct paths

# Loading each agency separately

# ACER
acer <- str_c(getwd(), 
                   "/",
                   str_subset(files, "acer[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ACER")

# BEREC OFFICE
berec <- str_c(getwd(), 
              "/",
              str_subset(files, "berec[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "BEREC")

# CEDEFOP
cedefop <- str_c(getwd(), 
               "/",
               str_subset(files, "cedefop[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "CEDEFOP")

# CPVO
## cpvo <- str_c(getwd(), 
##                 "/",
##                  str_subset(files, "cpvo[:digit:]")) %>% # Path to htmls
##   map(FactivaSource) %>% 
## map(Corpus, readerControl = list(language = NA)) %>% 
##  map_dfr(tidy) %>% 
##  mutate(date = as.POSIXct(datetimestamp),
##         acronym = "CPVO")

# INCORRECT: data needs not be dowloaded again

# EASA
easa <- str_c(getwd(), 
                 "/",
                 str_subset(files, "easa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASA")

# EASO

# EBA

#ECDC

# ECHA

# EEA

# EFCA

# EFSA

# EIGE

# EIOPA

# EMA

# EMCDDA

# EMSA

# ENISA

# ERA

# ESMA

# EUIPO

# EUROFOUND

# FRA

# Frontex

# EU-OSHA

# SRB

# Media total
dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
media <- do.call(rbind, mget(names(dfs)[dfs])) # Bind dataframes

media.month <- media %>%
  group_by(acronym,
           month = cut(datetimestam, "month"),
           .drop = FALSE) %>%
  summarise(media = n(),
            .groups = "keep")

# Plot time 
df.month %>% ggplot(aes(x = month, y = media)) +
  facet_wrap(~acronym,
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
  xlab("Time") + ylab("Count") + labs(title = "Appearances in the media (07/15 - 06/21)")



# Export
write.csv(media, "media.csv")