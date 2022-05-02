# Library
library(dplyr)
library(lmtest)
library(glmmTMB)
library(GLMMadaptive)
library(texreg)
library(kableExtra)
library(misty)
library(lubridate)
library(tidyr)
library(sjmisc)
library(tidyselect)
library(performance)

# Set time zone
Sys.time()
Sys.timezone()

load("./data/response_tweet2.Rda")

# ======================================================
#           Centering, transformation & description
# ======================================================

# Which agencies have been most responsive?
# ======================================================
response.tweet %>%
  group_by(agencyname) %>%
  summarize(total = n(),
            response = sum(response == 1),
            prop = response / total) %>%
  arrange(desc(prop)) %>%
  kable(col.names = c("Agency",
                      "Total",
                      "Response",
                      "Response (%)"),
        digits = 3) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")

# Transformation
# ======================================================
# Remove tweets with no sentiment score
response.tweet2 <- response.tweet %>%
  tidyr::drop_na(weighted.score, # No sentiment produced for some tweets
                 date.time) # One tweet was posted during change between summer/winter time (which causes NA)

end <- ymd_hms("2021-12-31 23:59:59", tz = "UTC")

response.data <- response.tweet2 %>%
  # filter(agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(
         
         # Log
         conversations.log = log(conversations),
         likes.log = log(like_count + 1),
         retweets.log = log(retweet_count + 1),
         
         # Factor  
         year.factor = as.factor(year),
         
         # Centering
         # IVs
         anomaly7.CWC = misty::center(anomaly7, type = "CWC", response.tweet2$agencyname),
         anomaly7.GMC = center(anomaly7),
         
         weigthed.score.CWC =  misty::center(weighted.score, type = "CWC", response.tweet2$agencyname),
         weigthed.score.CGM = center(weighted.score),
         
         # user comment controls
         conversations.log.CGM = center(conversations.log),
         likes.log.CGM = center(likes.log),
         retweet.log.CGM = center(retweets.log),
         short.CGM = center(short),
         weekend.CGM = center(weekend),
         qm.comment.CGM = center(qm.comment),
         attachment.CGM = center(attachment),
         same.message.CGM = center(same.message),
         uncivil.tweet.CGM = center(uncivil.tweet),
         office.hours.CGM = center(office.hours),
         
         # agency tweet controls
         qm.agency.CGM = center(qm.agency),
         mention.CGM = center(mention),
         
         # Serial autocorrelation
         # Time on Twitter
         join.numeric = time_length(difftime(end, # UTC
                                             ymd_hms(date.time)), # ymd_hms converts object to UTC
                                            "years"), # Take difference end of 2021 and timestamp of tweet
         join.numeric.pos = (join.numeric * -1) + max(join.numeric) , # Rescale so that earliest tweet has lowerst score and earliest tweet has value of 0
         join.ou = numFactor(join.numeric.pos),
         
         # Real time
         day.numeric = time_length(difftime(ymd_hms(date.time), 
                                            join.day), # NOTE: this is the date on which the first AGENCY tweet was recorded.  
                                            "years"),
         day.ou = numFactor(day.numeric)) # Diff between day + time on which comment was made and joining date

# Descriptives
# ======================================================
var.table.t <- response.data %>% 
  dplyr::select(where(is.numeric)) %>%
  descr() %>% 
  data.frame() %>% 
  dplyr::select(-type,
                -var,
                -se,
                -trimmed,
                -iqr) %>%
  mutate_at(vars(c("mean","sd", "NA.prc", "md")),funs(round(.,2)))

var.table.t %>%
  kable(col.names = c("Var",
                       "n",
                      "NA %",
                      "Mean",
                      "SD",
                      "Median",
                      "Range",
                      "Skew")) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")

# Correlations
response.data %>% 
  select(vars_select(names(response.data),
                     ends_with(c('CWC', 'CGM')),
                    starts_with('response'))) %>%
  tab_corr()

# ======================================================
#            Random
# ======================================================
# Random intercept
r1.0 <- glmmTMB(response ~
                (1 | agencyname),
              family = "binomial",
              data = response.data)
summary(r1.0)
icc(r1.0)

# UO: time since first day
r1.1 <- glmmTMB(response ~
                  (1 | agencyname) +
                  ou(day.ou + 0 | agencyname),
                family = "binomial",
                data = response.data)

summary(r1.1)
r1.1$sdr$pdHess ## Converged ?
anova(r1.0, r1.1)

# UO: time since joining date
r1.2 <- glmmTMB(response ~
                  short +
                  qm.comment +
                  attachment +
                  sentiment +
                  weekend +
                  office.hours + 
                  year +
                  (1 | agencyname) +
                  ou(join.ou + 0 | agencyname),
                family = "binomial",
                data = response.data)

summary(r1.2)
r1.2$sdr$pdHess ## Converged ?
anova(r1.1, r1.2)

# Compare with ar1: first day
r1.3 <- glmmTMB(response ~
                  short +
                  qm.comment +
                  attachment +
                  sentiment +
                  weekend +
                  office.hours + 
                  year +
                  (1 | agencyname) +
                  ar1(day.ou + 0 | agencyname),
                family = "binomial",
                data = response.data)

summary(r1.3)
anova(r1.1, r1.3)
tab_model(r1.1, r1.3)

# Compare with ar1: join
r1.4 <- glmmTMB(response ~
                  short +
                  qm.comment +
                  attachment +
                  sentiment +
                  weekend +
                  office.hours + 
                  year +
                  (1 | agencyname) +
                  ar1(join.ou + 0 | agencyname),
                family = "binomial",
                data = response.data)

summary(r1.4)
anova(r1.2, r1.4)
tab_model(r1.2, r1.4)
=======
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
>>>>>>> 104e69cb0f93ba3ce91fb8f612ba1da49938b05c
=======
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
>>>>>>> 104e69cb0f93ba3ce91fb8f612ba1da49938b05c
