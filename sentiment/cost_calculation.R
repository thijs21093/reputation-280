# libraries
library(googleLanguageR)

#load documents
load("media/media.Rdata")
<<<<<<< HEAD
load("Data/final_twitter_data/to_tweets_3")
load("Data/final_twitter_data/from_tweets_3")


#only keep english tweets
tibble.from <- tibble.from[tibble.from$lang=="en",]
tibble.to <- tibble.to %>% filter(referenced_type == "replied_to" & # Remove quoted tweets/retweets
                                    in_reply_to_user_id != author_id & # Remove tweets to self
                                    lang == "en")
=======
load("Data/data_replies_2/to_tweets")
load("Data/data_tweets_2/from_tweets")

#only keep english tweets
tibble.from <- tibble.from[tibble.from$lang=="en",]
tibble.to <- tibble.to[tibble.to$lang=="en",]
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

### MEDIA ###
#calculate costs for Google Language API
media$charlength <- nchar(media$text)
media$cost <- ceiling(media$charlength/1000)

### price structure = first 5000 units are free, then 2$ per 1000 units until 1M units (for entity sentiment)
((sum(media$cost)-5000)/1000)*2 # 134.28$


### TWEETS ###
#from
tibble.from$charlength <- nchar(tibble.from$text)
tibble.from$cost <- ceiling(tibble.from$charlength/1000)
# price structure = first 5000 units are free, then 2$ per 1000 units until 1M units
((sum(tibble.from$cost)-5000)/1000)*2 # 289.04$

#to
tibble.to$charlength <- nchar(tibble.to$text)
tibble.to$cost <- ceiling(tibble.to$charlength/1000)
# price structure = first 5000 units are free, then 1$ per 1000 units until 1M units (for tweet sentiment)
((sum(tibble.to$cost)-5000)/1000)*1 # 56.01$

### If we use sentiment for tibble.to and entity sentiment for media, then it should be around 134 + 56 = ~200$ 
### Typed the same in the google pricing calculator and it gave me a similar estimate: https://cloud.google.com/products/calculator#id=f8571a86-6c5c-44a2-819e-05c313a2c125


