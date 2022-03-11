#library(devtools)
#devtools::install_github("ropensci/googleLanguageR")

GL_AUTH="r-things-309a63b918c1.json"
library(googleLanguageR)
gl_auth(GL_AUTH)

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
library(tm)


#load documents
load("media/media.Rdata")
load("from_tweets_2")
load("to_tweets_2")

#only keep english tweets
tibble.from <- tibble.from[tibble.from$lang=="en",]
tibble.to <- tibble.to[tibble.to$lang=="en",]

# combine headings and text in media df to retrieve as many references to the agencies as possible
media$complete_text <- paste(paste0(media$heading, "."), media$text)
media$complete_text <- gsub("\r?\n|\r", " ", media$complete_text)

media$complete_text[1]

media.decode <- media %>% filter() %>% mutate(text = HTMLdecode(text))
media.decode$text

### TRIAL for media articles ###

#take first 100 media articles
media_trial <- media[media$id=="frontex",][1:100,]
media_trial$id <- as.character(c(1:100))

#pass to gcs
trial_results <- gl_nlp(
  media_trial$complete_text,
  nlp_type = "analyzeEntitySentiment",
  type = c("PLAIN_TEXT"),
  language = c("en")
)

#save(trial_results, file="sentiment/mediasentiment_output.RData")

#extract results to a usable format
testlist <- trial_results
for (i in 1:length(trial_results$entities)){
  print(i)
  trial_results$entities[[i]]$id = paste0(i)
}

trial_dataframe <-bind_rows(trial_results$entities)
trial_dataframe$name <- tolower(trial_dataframe$name)
trial_dataframe <- trial_dataframe %>%
  drop_na(score)

names <- trial_dataframe %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

agency.names <- names %>% filter(str_detect(name, "agency") == TRUE |
                                   str_detect(name, "frontex") == TRUE)
print(agency.names$name)

# only keep matches with the agency
agency_descriptives <- c("frontex",
                         "frontex plus",
                         "agency",                                                       
                         "border agency",                                             
                         "coast guard agency" ,                                          
                         "border protection agency",                                    
                         "border control agency",                                       
                         "border management agency",                               
                         "border patrol agency",                                        
                         "border security agency",                                       
                         "border-control agency",                                        
                         "border-guard agency",                                          
                         "border enforcement agency",                                    
                         "border guard agency",                                          
                         "coast-guard agency",                                           
                         "european agency",                                              
                         "european agency for the management of operational cooperation",
                         "frontier agency")

trial_dataframe2 <- trial_dataframe[trial_dataframe$name %in% agency_descriptives,]

# introduce combined measure of magnitude and score to avoid misclassifications
trial_dataframe$com_score <- trial_dataframe$magnitude*trial_dataframe$score

ggplot(trial_dataframe, aes(x = com_score)) +  
  geom_histogram(colour="black", fill="white")

# best approach seems to be to measure positivity and negativity separately
# Let's take the mention with the lowest score to see differences.
trial_dataframe <- trial_dataframe %>% 
  group_by(id) %>% 
  summarise(com_score_mean = mean(com_score),
            com_score_min = min(com_score),
            com_score_max = max(com_score),
            hits = n())

ggplot(trial_dataframe, aes(x = com_score_mean)) +  
  geom_histogram(colour = "black", fill = "white")

# join text and score dataset
media_sentiment <- left_join(media_trial, trial_dataframe, by="id")

# save file
save(media_sentiment, file="sentiment/media_sentiment.RData")

### TRIAL for tweets ###
# Take first 10 tweets

tweets_trial <- tibble.to[1:10,]

# Pass to gcs
tweets_results <- gl_nlp(
  tweets_trial$text,
  nlp_type = "analyzeSentiment",
  type = c("PLAIN_TEXT"),
  language = c("en")
)

#add sentiment to tweet dataset
tweets_trial$sentiment <- tweets_results$documentSentiment$score 
tweets_trial$magnitude <- tweets_results$documentSentiment$magnitude 

save(tweets_trial, file="sentiment/tweets_sentiment.RData")
save(tweets_results, file="sentiment/tweetssentiment_output.RData")
