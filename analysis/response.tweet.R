# Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(sjmisc)
library(kableExtra)
library(fixest)
library(modelsummary)
library(performance)
library(parameters)
library(lmtest)
library(sandwich)
library(texreg)
library(ggeffects)
<<<<<<< HEAD


load("./data/response_tweet.Rda")

# Set time zone
Sys.setenv(TZ = 'GMT')
=======
library(panelr)
library(glmmTMB)
library(lubridate)

load("./data/response_tweet2.Rda")



cubic.root <- function(x) {
  sign(x) * abs(x)^(1/3)
}
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# ======================================================
#           Centering, transformation & description
# ======================================================
<<<<<<< HEAD

=======
<<<<<<< HEAD

# Transformation
# ======================================================
response.data <- response.tweet %>%
  filter(same.message == 0 & # Difficult to model, so better to remove altogether
         agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(
         
         # Misc.
         conversations.log = log2(conversations),
         sentiment.factor = as.factor(sentiment),
         agencyyear = paste0(agencyname, "-", year),
         
         # Media count
         media.count.30d.log = log2(media.count.30d + 1),
         media.count.90d.log = log2(media.count.90d + 1),
         media.count.365d.log = log2(media.count.365d + 1),
         
         # Scale information
         information.1w.s = information.1w/100,
         information.30d.s = information.30d/100,
         information.90d.s = information.90d/100,
         information.365d.s = information.365d/100,
         
         # Scale Twitter index
         twitter.index.30d.s = twitter.index.30d/100,
         twitter.index.90d.s = twitter.index.90d/100,
         twitter.index.365d.s = twitter.index.365d/100,
=======
>>>>>>> 104e69cb0f93ba3ce91fb8f612ba1da49938b05c
# Transformation
# ======================================================
response.data <- response.tweet %>%
  filter(agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(
         
         # Misc.
        # conversations.log = log2(conversations),
         sentiment.factor = as.factor(sentiment),

         # Log
         media.count.1w.log = log2(media.count.1w + 1),
         twitter.index.1w.cr = cubic.root(twitter.index.1w),
         comments.1w.log = log(comments.1w + 1),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
         
         # Scale time on Twitter
         time.on.Twitter.s = time.on.Twitter/365,
         time.on.Twitter.s2 = time.on.Twitter.s^2,

          day.factor = as.factor(day), 
         
          # NOTE: replace with created_at
          day.numeric = time_length(difftime(day, min(day)), "years"), # Take difference between day and first day in dataset and compute lenght in years
          day.ou = numFactor(day.numeric),
          join.numeric = time_length(difftime(day, join.day), "months"), # Diff between day on which comment was made and joining date
          join.ou = numFactor(join.numeric))

# Descriptives
# ======================================================
var.table.t <- response.data %>% 
  dplyr::select(where(is.numeric)) %>%
  descr() %>% 
  data.frame() %>% 
  dplyr::select(-type,
                -var,
                -NA.prc,
                -se,
                -trimmed,
                -iqr) %>%
  mutate_at(vars(c("mean","sd","md")),funs(round(.,3)))


var.table.t %>%
  kable(col.names = c("Var",
                       "n",
                      "Mean",
                      "SD",
                      "Median",
                      "Range",
                      "Skew")) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")

# Which agencies have been most responsive?
# ======================================================
response.data %>%
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

# ======================================================
#           Fixed effect
# ======================================================

<<<<<<< HEAD
# Start with 90 days
f1.0 <- feglm(response ~ 
                twitter.index.90d.s + 
                media.valence.90d +
                media.count.90d.log, # Log doesn't improve fit with agency*year FE, but it does with agency FE and agency-year FE
              family = "logit",
              data = response.data) 

# Model 1.1: Adding controls without missing
f1.1 <- update(f1.0, . ~ . +
                 short +
                 qm.comment +
                 attachment +
                 conversations.log + # Log much better with agency-year and agency*year FE
                 information.90d.s	+ # 90 days instead of 30 days doesn't matter with agency*year FE, but it improve fit with agency FE and agency-year FE 
                 i(sentiment, ref = "twitter.neutral") +
                 weekend,
              family = "logit") 

etable(f1.0, f1.1,
       fitstat = ~ . + ll + aic) # Fit statistics

# Model 1.2: agency fixed effects
f1.2 <- feglm(response ~ twitter.index.90d.s + 
              media.valence.90d +
              media.count.90d.log +
              short +
              qm.comment +
              attachment +
              conversations.log +
              information.90d.s	+ 
              i(sentiment, ref = "twitter.neutral") + 
              weekend | 
=======
# Start with 1 week
f1.0 <- feglm(response ~ 
                twitter.valence.1w + 
                comments.1w +
                media.count.1w, 
              family = "logit",
              data = response.data) 

summary(f1.0)

# Model 1.1: add interactions
f1.1 <- feglm(response ~ 
                twitter.valence.1w*comments.1w*media.count.1w, 
              family = "logit",
              data = response.data) 

summary(f1.1)

# Model 1.2: Adding controls without missing
f1.2 <- update(f1.1, . ~ . +
                 short +
                 qm.comment +
                 attachment +
                 uncivil.tweet +
                 i(sentiment, ref = "twitter.neutral") +
                 weekend +
                 office.hours,
              family = "logit") 

etable(f1.1, f1.2,
       fitstat = ~ . + ll + aic) # Fit statistics

# Model 1.3: agency fixed effects
f1.3 <- feglm(response ~ 
                twitter.valence.1w*comments.1w*media.count.1w +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours | 
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
              agencyname,
              family = "logit",
              data = response.data) 

<<<<<<< HEAD
# Model 1.3: agency and year fixed effects
f1.3 <- feglm(response ~ 
                twitter.index.90d.s + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d.s	+
                i(sentiment, ref = "twitter.neutral") +
                weekend | 
                agencyname + year,
              family = "logit",
              data = response.data) 

# Model 1.4: time x agency interaction as FE effect
f1.4 <- feglm(response ~ 
                twitter.index.90d.s + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d.s	+
                i(sentiment, ref = "twitter.neutral") +
                weekend | 
=======
summary(f1.3)


# Model comparison
etable(f1.2, f1.3,
       fitstat = ~ . + ll + aic) # Fit statistics

# Model 1.4: agency and year fixed effects
f1.4 <- feglm(response ~ 
                twitter.valence.1w*comments.1w*media.count.1w +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours  | 
                agencyname + year,
              family = "logit",
              data = response.data)

summary(f1.4)

# Model 1.5: agency^year fixed effects
f1.5 <- feglm(response ~ 
                twitter.valence.1w*comments.1w*media.count.1w +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours  | 
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
                agencyname^year,
              family = "logit",
              data = response.data) 

<<<<<<< HEAD
# Testing transformation

# Extract dataframe from model 1.4 and rerun 1.2 and 1.3 with that data
obs.removed <- f1.4$obs_selection$obsRemoved
data.removed <- response.data[(obs.removed),]

# Agency FE
f1.2m <- update(f1.2, . ~ . , data = data.removed) 
f1.2c <- update(f1.2m, . ~ . - information.90d.s + information.30d.s) 

etable(f1.2m, f1.2c, fitstat = ~ . + ll + aic)

# Agency-year FE
f1.3m <- update(f1.3, . ~ . , data = data.removed) 
f1.3c <- update(f1.3m, . ~ . - information.90d.s + information.30d.s) 
etable(f1.3m, f1.3c, fitstat = ~ . + ll + aic) 

# Agency x year FE
f1.4c <- update(f1.4, . ~ . - information.90d.s + information.30d.s)
etable(f1.4, f1.4c, fitstat = ~ . + ll + aic) # Fit statistics

# Model comparison
etable(f1.2m, f1.3m, f1.4,
=======
summary(f1.5)

# Model 1.6: agency + year fixed effects, but only agency clustered errors
f1.6 <- feglm(response ~ 
                twitter.valence.1w*comments.1w*media.count.1w +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours  | 
                agencyname + year,
              cluster = c("agencyname", "year"),
              family = "logit",
              data = response.data) 

summary(f1.6)

# Model comparison
etable(f1.4, f1.5, f1.6,
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
       fitstat = ~ . + ll + aic) # Fit statistics

# Which model has the best fit indices?
# Model 1.3

<<<<<<< HEAD
# Model 1.5: Adding time on Twitter
f1.5 <- update(f1.2m,  . ~ . + time.on.Twitter.s)

etable(f1.2m, f1.5, fitstat = ~ . + ll + aic) # Much better fit

f1.5.rawpoly <- update(f1.2m,  . ~ . + poly(time.on.Twitter.s, 2, raw = TRUE)) # Centering? 
f1.5.orgpoly <- update(f1.2m,  . ~ . + poly(time.on.Twitter.s, 2))
f1.5alt <- update(f1.2m,  . ~ . + time.on.Twitter.s + time.on.Twitter.s2)

etable(f1.5, f1.5.rawpoly, f1.5.orgpoly, fitstat = ~ . + ll + aic) # Second order polynomials are n.s

# Model 1.7: Time on Twitter as random slope
# Assumes linear relationshio -> see https://stats.stackexchange.com/questions/548268/can-i-include-time-as-an-independent-variable-in-a-panel-data-model
f1.7 <- feglm(response ~ 
                twitter.index.90d.s + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d.s	+ 
                i(sentiment, ref = "twitter.neutral") + 
                weekend |
                agencyname[time.on.Twitter.s],
              family = "logit",
              data = data.removed)

etable(f1.5, f1.7, fitstat = ~ . + ll + aic)  # Big importoment in terms of AIC/LL, but slightly worse BIC

# Model 1.8: Continuous time on Twitter + poly as random slope
f1.8 <- feglm(response ~ 
                twitter.index.90d.s + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d.s	+ 
                i(sentiment, ref = "twitter.neutral") + 
                weekend |
                agencyname[time.on.Twitter.s, time.on.Twitter.s2],
              family = "logit",
              data = data.removed)

etable(f1.7, f1.8, fitstat = ~ . + ll + aic)  # Only one varying slope seems to be most parsimonious

# Select model with best fit
etable(f1.3m, f1.7, fitstat = ~ . + ll + aic) # Again, varying slope outperforms agency FE + year FE

# Try different lags
f1.7month <- update(f1.7, . ~ . - 
                      twitter.index.90d.s - 
                      media.valence.90d -
                      media.count.90d.log -
                      information.90d.s	+

                      twitter.index.30d.s +
                      media.valence.30d +
                      media.count.30d.log +
                      information.30d.s)

f1.7year <- update(f1.7, . ~ . - 
                     twitter.index.90d.s - 
                     media.valence.90d -
                     media.count.90d.log -
                     information.90d.s	+

                     twitter.index.365d.s +
                     media.valence.365d +
                     media.count.365d.log +
                     information.365d.s)

obs.removed2 <- f1.7year$obs_selection$obsRemoved
data.removed2 <- data.removed[(obs.removed2),]

f1.7m <- update(f1.7, . ~ . , data = data.removed2)
f1.7monthm <- update(f1.7month, . ~ . , data = data.removed2)

etable(f1.7monthm, f1.7m,  f1.7year, fitstat = ~ . + ll + aic) # Year lag has lowest BIC, but better to make an theoretically informed choice.
                                                               # For now, let's use 3 months for all vars.
                                                               # We might even use different time lags for different vars,
                                                               # As some processes might be faster than others.

# Model 1.8: Adding controls with some missingness
f1.8 <- update(f1.7m, . ~ . +
                mention +
                qm.agency,
                data = response.data) 

etable(f1.8, fitstat = ~ . + ll + aic)

# Clustering

# Model 1.8: conversation as cluster
f1.8.conversation <- update(f1.8, cluster = ~ conversation_id)
f1.8.agencyyear <- update(f1.8, cluster = ~ agencyname^year)

etable(f1.8, f1.8.conversation, f1.8.agencyyear, fitstat = ~ . + ll + aic)

# Quick export
# Add info on varying slope

models <- list(
  # "Model 1.1 (cluster = agency)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log |
  #                  agencyname + agencyname[[time.on.Twitter.s]], 
  #                  data = response.data, family = binomial(link = "logit")),
  #"Model 2.1 (cluster = conversation)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log | 
  #                  agencyname + agencyname[[time.on.Twitter.s]], 
  #                  data = response.data, family = binomial(link = "logit"), cluster = ~conversation_id),
  #"Model 3.1 (cluster = agency-year)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log |
  #                  agencyname + agencyname[time.on.Twitter.s], 
  #                  data = response.data, family = binomial(link = "logit"), cluster = ~agencyname^year),
  "Model 1.2 (cluster = agency)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log +
                      short + qm.comment + attachment + conversations.log + information.90d.s + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency | 
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit")),
  "Model 2.2 (cluster = conversation)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log + 
                      short + qm.comment + attachment + conversations.log + information.90d.s + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency |
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~conversation_id),
  "Model 3.2 (cluster = agency-year)" = feglm(fml = response ~ twitter.index.90d.s + media.valence.90d + media.count.90d.log +
                      short + qm.comment + attachment + conversations.log + information.90d.s + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency |
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~agencyname^year))

modelsummary(models,
             estimate = "{estimate} ({std.error})",
             statistic = "p = {p.value}") # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

# ======================================================
#           # GLM for model evaluation/comparison
# ======================================================

# Construct dataset with same observation
obs.removed3 <- f1.8$obs_selection$obsRemoved
data.removed3 <- response.data[(obs.removed3),]

# Demean data
data.demean <- cbind(
  data.removed3,
  datawizard::demean(data.removed3,
                     select = c("twitter.index.90d.s", "media.valence.90d", "media.count.90d.log",
                                "short", "time.on.Twitter.s", "qm.comment", "attachment", "conversations.log", "sentiment.factor", "information.90d.s", "weekend", "mention", "qm.agency"), group = "agencyname"))

glm1.8 <- glm(response ~ 0 + # Remove intercept
                twitter.index.90d.s_within +
                media.valence.90d_within +
                media.count.90d.log_within +
                short_within + 
                qm.comment_within + 
                attachment_within +
                conversations.log_within + 
                information.90d.s_within +
                sentiment.factor_twitter.criticism_within +
                sentiment.factor_twitter.praise_within +
                weekend_within + 
                mention_within +
                qm.agency_within +
                agencyname/time.on.Twitter.s_within,
                data = data.demean,
                family = binomial())

summary(glm1.8)

# Model Comparison
anova(glm1.8, test = "LRT") # Likelihood Ratio test of all vars

glm1.8b <- update(glm1.8, . ~ . -
                  agencyname/time.on.Twitter.s_within +
                  agencyname)

test_performance(glm1.8b, glm1.8) 
test_likelihoodratio(glm1.8b, glm1.8)

# Plotting
# Note: SE are not clustered

# Twitter valence
p1 <- ggpredict(glm1.8,terms = "twitter.index.90d.s_within [all]")
plot(p1) + scale_y_continuous(labels = scales::percent_format())

# Media valence
p2 <- ggpredict(glm1.8, terms = "media.valence.90d_within  [all]")
plot(p2) + scale_y_continuous(labels = scales::percent_format())

# Media count
p3 <- ggpredict(glm1.8, terms = "media.count.90d.log_within  [all]")
plot(p3) + scale_y_continuous(labels = scales::percent_format())

# Proportion of cases correctly predicted.
data.demean$fitted.1 <- fitted(glm1.8)
data.demean <- data.demean %>%  mutate(resp.1 = ifelse(fitted.1 > 0.5, 1, 0)) # Convert probabilities to actual responses (0 or 1)
prop.table(table(data.demean$response, # How are we doing in terms of correct classification?
                 data.demean$resp.1), 1) # show the table in a format where rows sum up to 1

# Exporting models
htmlreg(list(coeftest(glm1.8, vcovCL(glm1.8, cluster= ~ agencyname)),
               coeftest(glm1.8, vcovCL(glm1.8, cluster= ~ conversation_id)),
               coeftest(glm1.8, vcovCL(glm1.8, cluster= ~ agencyyear))),
          custom.model.names = c('Cluster = agency', 'Cluster = conversation', 'Cluster = agency-year'),
        omit.coef = "agencyname",
        digits = 3,
        include.pvalues = TRUE,
        file = "glm.html")
=======
# Model 1.5: Adding controls with some missingness
f1.7 <- update(f1.4, . ~ . +
                mention +
                qm.agency,
                data = response.data) 

etable(f1.7, fitstat = ~ . + ll + aic)


models <- list(
  "Model 1.0" = feglm(response ~ 
                        twitter.valence.1w + comments.1w + media.count.1w | 
                        agencyname + year,
                      family = "logit",
                      data = response.data),
  "Model 1.1" = feglm(response ~ 
                        twitter.valence.1w*comments.1w*media.count.1w| 
                        agencyname + year,
                      family = "logit",
                      data = response.data),
  "Model 1.2" = feglm(response ~ 
                        twitter.valence.1w*comments.1w*media.count.1w +
                        short +
                        qm.comment +
                        attachment +
                        uncivil.tweet +
                        i(sentiment, ref = "twitter.neutral") +
                        weekend +
                        office.hours  | 
                        agencyname + year,
                      family = "logit",
                      data = response.data))

modelsummary(models,
             estimate = "{estimate} ({std.error})",
             statistic = "p = {p.value}") # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

plot_model(f1.4, type = "pred", terms = c("comments.1w [all]", "twitter.valence.1w", "media.count.1w"))

models2 <- list(
  "Model 2.0" = feglm(response ~ 
                        twitter.index.1w + media.count.1w | 
                        agencyname + year,
                      family = "logit",
                      data = response.data),
  "Model 2.1" = feglm(response ~ 
                        twitter.index.1w*media.count.1w | 
                        agencyname + year,
                      family = "logit",
                      data = response.data),
  "Model 2.2" = feglm(response ~ 
                        twitter.index.1w*media.count.1w +
                        short +
                        qm.comment +
                        attachment +
                        uncivil.tweet +
                        i(sentiment, ref = "twitter.neutral") +
                        weekend +
                        office.hours  | 
                        agencyname + year,
                      family = "logit",
                      data = response.data))

modelsummary(models2,
             estimate = "{estimate} ({std.error})",
             statistic = "p = {p.value}") # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

f2.2 <- feglm(response ~ 
                twitter.index.1w*media.count.1w +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours  | 
                agencyname + year,
              family = "logit",
              data = response.data)

plot_model(f2.2,
           type = "pred", 
           terms = c("twitter.index.1w [all]", "media.count.1w [quart]"))

f2.2a <- feglm(response ~ 
                media.count.1w.log*twitter.index.cr +
                short +
                qm.comment +
                attachment +
                uncivil.tweet +
                i(sentiment, ref = "twitter.neutral") +
                weekend +
                office.hours  | 
                agencyname + year,
              family = "logit",
              data = response.data)

etable(f2.2, f2.2a,
       fitstat = ~ . + ll + aic) # Fit statistics
summary(f2.2a)

plot_model(f2.2a,
           type = "pred", 
           terms = c("media.count.1w.log [all]", "twitter.valence.1w [quart]", "comments.1w.log [meansd]"))
<<<<<<< HEAD

# ======================================================
#            Random
# ======================================================
r1.0 <- glmmTMB(response ~
                short +
                qm.comment +
                attachment +
                sentiment +
                weekend +
                office.hours + 
                year + 
                (1 | agencyname),
              family = "binomial",
              data = response.data)
summary(r1.0)

# UO: time since first day
r1.1 <- glmmTMB(response ~
                  short +
                  qm.comment +
                  attachment +
                  sentiment +
                  weekend +
                  office.hours + 
                  year +
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
