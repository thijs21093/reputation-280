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

# Set time zone
Sys.setenv(TZ = 'GMT')

#load documents
load("media/media_articles.Rdata")
load("from_tweets_3")
load("to_tweets_3")


#only keep english tweets
tibble.from <- tibble.from[tibble.from$lang=="en",]
tibble.to <- tibble.to %>% filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
                               in_reply_to_user_id != author_id & # Remove tweets to self
                               lang == "en")

#clean text of tweets
tibble.to$text_new <- str_replace_all(tibble.to$text," "," ") %>% str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>% str_remove_all("@[[:alnum:]_]{4,}") %>% str_replace_all("&amp;", "and") %>%  str_trim("both") %>% str_remove_all("^RT:? ")


### CLASSIFY TO.TWEETS
tweet_results <- split(tibble.to, tibble.to$agencyhandle)

#try small sample
#tweetlist_sample <- list()
#set.seed(1993)
#for (i in 1:length(tweet_results)){
  tweetlist_sample[[i]] <- tweet_results[[i]][sample(nrow(tweet_results[[i]]), 10), ]
}

#tweet_output <- list()
#tweet_output_csvs <- list()

#below loop is run in chunks. done so far=1:3, 4:6, 7:15, 16:26
for (i in 16:26) {
  print(paste0(tweet_results[[i]][1, ]$agencyhandle))
  data <- tweet_results[[i]]
  output <- gl_nlp(
    data$text_new,
    nlp_type = "analyzeSentiment",
    type = c("HTML"),
    language = c("en")
  )
  output_df <-
    data.frame(
      text = output$text,
      magnitude = output$documentSentiment$magnitude,
      score = output$documentSentiment$score,
      name = paste0(tweet_results[[i]][1, ]$agencyhandle)
    ) #create df from output list
  tweet_output[[i]] <- output #add list to list of lists
  tweet_output_csvs[[i]] <- output_df #add list to list of csvs
  save(output, file = paste0(
    "sentiment/data/",
    paste0(tweet_results[[i]][1, ]$agencyhandle),
    ".RData"
  )) #save current batch of HDD
  write.csv(output_df, file = paste0(
    "sentiment/data/",
    paste0(tweet_results[[i]][1, ]$agencyhandle),
    ".csv"
  )) #save current batch on HDD
}

save(tweet_output, file="sentiment/data/tweetoutput.RData")
save(tweet_output_csvs, file="sentiment/data/tweetoutput_csv.RData")

# merge output and original data
for (i in 1:length(tweet_results)){
  tweet_results[[i]]$score <- tweet_output_csvs[[i]]$score
  tweet_results[[i]]$magnitude <- tweet_output_csvs[[i]]$magnitude
  tweet_results[[i]]$reftext <-  tweet_output_csvs[[i]]$text
  tweet_results[[i]]$refagency <- tweet_output_csvs[[i]]$name
}

tweetsTO_final <- bind_rows(tweet_results)

save(tweetsTO_final, file="sentiment/data/tweetsTO_final.RData")

# combine headings and text in media df to retrieve as many references to the agencies as possible
media$complete_text <- paste(paste0(media$heading, "."), media$text)
media$complete_text <- gsub("\r?\n|\r", " ", media$complete_text)

media$complete_text[2]
write.csv(media$complete_text[c(1:10)], file="test.txt") # check how R and excel handle the escape character \ - turns out, if put in a .txt file, it turns it into two "" each time
### TRIAL for media articles ###

#take first 100 media articles
set.seed(1993)
media_trial <- sample_n(media[media$id=="ecdc",], 100)
media_trial$id <- as.character(c(1:100))

#pass to gcs
trial_results_ecdc <- gl_nlp(
  media_trial$complete_text,
  nlp_type = "analyzeEntitySentiment",
  type = c("PLAIN_TEXT"),
  language = c("en")
)

#save(trial_results_ecdc, file="sentiment/mediasentimentecdc_output.RData")

#extract results to a usable format
testlist <- trial_results_ecdc
for (i in 1:length(testlist$entities)){
  print(i)
  testlist$entities[[i]]$id = paste0(i)
}

trial_dataframe <- bind_rows(testlist$entities)
trial_dataframe$name <- tolower(trial_dataframe$name)
trial_dataframe <- trial_dataframe %>%
  drop_na(score)

names <- trial_dataframe %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

agency.names <- names %>% filter(str_detect(name, "agency") == TRUE |
                                   str_detect(name, "ecdc") == TRUE |
                                   str_detect(name, "centre") == TRUE |
                                   str_detect(name, "center") == TRUE)
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

agency_descriptives_ecdc <- c("ecdc",
                              "european center for disease prevention and control",
                              "european centre for disease prevention and control",
                              "disease control agency",
                              "european centre for disease control",
                              "theeuropean center for disease prevention and control",
                              "agency")

trial_dataframe2 <- trial_dataframe[trial_dataframe$name %in% agency_descriptives,] #frontex
trial_dataframe3 <- trial_dataframe[trial_dataframe$name %in% agency_descriptives_ecdc,] #ecdc

# introduce combined measure of magnitude and score to avoid misclassifications
trial_dataframe3$com_score <- trial_dataframe3$magnitude*trial_dataframe3$score

ggplot(trial_dataframe3, aes(x = com_score)) +  
  geom_histogram(colour = "black", fill = "white")

#calculate more metrics
trial_dataframe3 <- trial_dataframe3 %>% 
  group_by(id) %>% 
  summarise(com_score_mean = mean(com_score),
            com_score_median = median(com_score),
            com_score_min = min(com_score),
            com_score_max = max(com_score),
            score_mean = mean(score),
            com_score_median = median(score),
            score_min = min(score),
            score_max = max(score),
            hits = n())

ggplot(trial_dataframe3, aes(x = com_score_mean)) +  
  geom_histogram(colour = "black", fill = "white")

# join text and score dataset
media_sentiment <- left_join(media_trial, trial_dataframe, by="id")

# save file
save(media_sentiment, file="sentiment/media_sentiment_ecdc.RData")

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
