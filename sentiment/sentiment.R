#library(devtools)
#devtools::install_github("ropensci/googleLanguageR")

GL_AUTH="r-things-309a63b918c1.json"
library(googleLanguageR)
gl_auth(GL_AUTH)

library(dplyr)

#load documents
load("media/media.Rdata")
load("Data/data_replies_2/to_tweets")
load("Data/data_tweets_2/from_tweets")

#only keep english tweets
tibble.from <- tibble.from[tibble.from$lang=="en",]
tibble.to <- tibble.to[tibble.to$lang=="en",]


# combine headings and text in media df to retrieve as many references to the agencies as possible
media$complete_text <- paste(paste0(media$heading, "."), media$text)

### TRIAL ###

#take first 100 media articles
media_trial <- media[media$id=="frontex",][1:100,]

#pass to gcs
trial_results <- gl_nlp(
  media_trial$complete_text,
  nlp_type = "analyzeEntitySentiment",
  type = c("PLAIN_TEXT"),
  language = c("en")
)

#extract results to a usable format
testlist <- trial_results
for (i in 1:length(trial_results$entities)){
  print(i)
  trial_results$entities[[i]]$id = paste0(i)
}

trial_dataframe <-bind_rows(trial_results$entities)
trial_dataframe$name <- tolower(trial_dataframe$name)

# only keep matches with the agency
agency_descriptives <- c("frontex", "border control agency", "border patrol agency", "coast guard agency", "border agency")
trial_dataframe <- trial_dataframe[trial_dataframe$name %in% agency_descriptives,]

# best approach seems to be to measure positivity and negativity separately
# Let's take the metion with the lowest score to see differences.
trial_dataframe <- trial_dataframe %>% 
  group_by(id) %>% 
  slice(which.min(score))
