library(tm.plugin.factiva)
library(tm)
library(purrr)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)

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
         acronym = "ACER",
         infodesc = as.list(infodesc))

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

# INCORRECT: data needs not be downloaded again

# EASA
corpus.easa <- str_c(getwd(), 
                 "/",
                 str_subset(files, "easa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>%
  map(tidy)
  
easa <- do.call(rbind.data.frame, corpus.easa) %>%
mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASA")

# EBA
eba <- str_c(getwd(), 
              "/",
              str_subset(files, "eba[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EBA")

# ECDC
ecdc <- str_c(getwd(), 
             "/",
             str_subset(files, "ecdc[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECDC")

# ECHA
echa <- str_c(getwd(), 
              "/",
              str_subset(files, "echa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECHA")

# EEA
eea <- str_c(getwd(), 
              "/",
              str_subset(files, "eea[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EEA")

# EFCA
efca <- str_c(getwd(), 
             "/",
             str_subset(files, "efca[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFCA",
         company = as.list(company))

# EFSA
corpus.efsa <- str_c(getwd(), 
                     "/",
                     str_subset(files, "efsa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>%
  map(tidy)

efsa <- do.call(rbind.data.frame, corpus.efsa) %>%
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFSA") 

# EIGE
eige <- str_c(getwd(), 
              "/",
              str_subset(files, "eige[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EIGE")
# EIOPA
eiopa <- str_c(getwd(), 
              "/",
              str_subset(files, "eiopa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EIOPA")

# EMA
## ema <- str_c(getwd(), 
##               "/",
##               str_subset(files, "ema[:digit:]")) %>% # Path to htmls
##  map(FactivaSource) %>% 
##  map(Corpus, readerControl = list(language = NA)) %>% 
##  map_dfr(tidy) %>% 
##  mutate(date = as.POSIXct(datetimestamp),
##         acronym = "EMA")

# INCORRECT: data needs not be downloaded again

# EMCDDA
emcdda <- str_c(getwd(), 
             "/",
             str_subset(files, "emcdda[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMCDDA")
# EMSA
emsa <- str_c(getwd(), 
                "/",
                str_subset(files, "emsa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMSA")

# ENISA
enisa <- str_c(getwd(), 
              "/",
              str_subset(files, "enisa[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ENISA",
         infodesc = as.list(infodesc))

# ERA
era <- str_c(getwd(), 
               "/",
               str_subset(files, "era[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ERA",
         industry = as.list(industry))

# ESMA
esma <- str_c(getwd(), 
             "/",
             str_subset(files, "esma[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ESMA")

# EUIPO
euipo <- str_c(getwd(), 
             "/",
             str_subset(files, "euipo[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUIPO")

# EUROFOUND
eurofound <- str_c(getwd(), 
               "/",
               str_subset(files, "eurofound[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROFOUND")

# FRA
corpus.fra <- str_c(getwd(), 
                     "/",
                     str_subset(files, "fra[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>%
  map(tidy)

fra <- do.call(rbind.data.frame, corpus.fra) %>%
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRA")

# Frontex
corpus.frontex <- str_c(getwd(), 
                    "/",
                    str_subset(files, "frontex[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>%
  map(tidy)

frontex <- do.call(rbind.data.frame, corpus.frontex) %>%
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX")

# EU-OSHA
euosha <- str_c(getwd(), 
                 "/",
                 str_subset(files, "euosha[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EU-OSHA")
# SRB
srb <- str_c(getwd(), 
                "/",
                str_subset(files, "srb[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "SRB")

# Media total
dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
media <- bind_rows(mget(names(dfs)[dfs]), .id = "id") # Bind rows


media.month <- media %>%
  group_by(acronym,
           month = cut(datetimestamp, "month"),
           .drop = FALSE) %>%
  summarise(media = n(),
            .groups = "keep") %>%
  mutate(month = as.Date(month)) %>%
  filter(month >= "2015-07-01" & # Start date
           month <= "2021-06-30") # End date

# Plot time 
media.month %>% ggplot(aes(x = month, y = media)) +
  facet_wrap(~acronym,
             ncol = 6,
             scales = "free") +
  geom_smooth(method = "loess",
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