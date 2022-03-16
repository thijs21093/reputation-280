# Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(sjmisc)
library(kableExtra)
library(fixest)
library(modelsummary)
library(GLMMadaptive)
library(performance)
library(parameters)

load("./data/response_tweet.Rda")

# Set time zone
Sys.setenv(TZ = 'GMT')

# ======================================================
#           Centering, transformation & description: cross-sectional
# ======================================================

# Transformation
# ======================================================
response.data <- response.tweet %>%
  filter(same.message == 0 & # Difficult to model, so better to remove altogether
         time.on.Twitter >= 0,
         agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(conversations.log = log(conversations),
         sentiment.factor = as.factor(sentiment),
         media.count.30d.log = log(media.count.30d + 1),
         media.count.90d.log = log(media.count.90d + 1),
         media.count.365d.log = log(media.count.365d + 1),
         information.1w.log = log(information.1w + 1),
         information.30d.log = log(information.30d + 1),
         information.90d.log = log(information.90d + 1),
         time.on.Twitter.s = time.on.Twitter/365,
         time.on.Twitter.s2 = time.on.Twitter.s^2)
  
# Centering & transformation: cross-sectional
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
#           Cross-sectional fixed effect
# ======================================================

# Start with 90 days
f1.0 <- feglm(response ~ 
                twitter.valence.90d + 
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
                 information.90d	+ # 90 days instead of 30 days doesn't matter with agency*year FE, but it improve fit with agency FE and agency-year FE 
                 i(sentiment, ref = "twitter.neutral") +
                 weekend,
              family = "logit") 

etable(f1.0, f1.1,
       fitstat = ~ . + ll + aic) # Fit statistics

# Model 1.2: agency fixed effects
f1.2 <- feglm(response ~ twitter.valence.90d + 
              media.valence.90d +
              media.count.90d.log +
              short +
              qm.comment +
              attachment +
              conversations.log +
              information.90d	+ 
              i(sentiment, ref = "twitter.neutral") + 
              weekend | 
              agencyname,
              family = "logit",
              data = response.data) 

# Model 1.3: agency and year fixed effects
f1.3 <- feglm(response ~ 
                twitter.valence.90d + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d	+
                i(sentiment, ref = "twitter.neutral") +
                weekend | 
                agencyname + year,
              family = "logit",
              data = response.data) 

# Model 1.4: time x agency interaction as FE effect
f1.4 <- feglm(response ~ 
                twitter.valence.90d + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d	+
                i(sentiment, ref = "twitter.neutral") +
                weekend | 
                agencyname^year,
              family = "logit",
              data = response.data) 

# Testing transformation

# Extract dataframe from model 1.4 and rerun 1.2 and 1.3 with that data
obs.removed <- f1.4$obs_selection$obsRemoved
data.removed <- response.data[(obs.removed),]

# Agency FE
f1.2m <- update(f1.2, . ~ . , data = data.removed) 
f1.2c <- update(f1.2m, . ~ . - information.90d + information.30d) 

etable(f1.2m, f1.2c, fitstat = ~ . + ll + aic)

# Agency-year FE
f1.3m <- update(f1.3, . ~ . , data = data.removed) 
f1.3c <- update(f1.3m, . ~ . - information.90d + information.30d) 
etable(f1.3m, f1.3c, fitstat = ~ . + ll + aic) 

# Agency x year FE
f1.4c <- update(f1.4, . ~ . - information.90d + information.30d)
etable(f1.4, f1.4c, fitstat = ~ . + ll + aic) # Fit statistics

# Model comparison
etable(f1.2m, f1.3m, f1.4,
       fitstat = ~ . + ll + aic) # Fit statistics

# Which model has the best fit indices?
# Model 1.3

# Model 1.5: Adding time on Twitter
f1.5 <- update(f1.2m,  . ~ . + time.on.Twitter.s)

etable(f1.2m, f1.5, fitstat = ~ . + ll + aic) # Much better fit

f1.5.rawpoly <- update(f1.2m,  . ~ . + poly(time.on.Twitter.s, 2, raw = TRUE)) # Centering? 
f1.5.orgpoly <- update(f1.2m,  . ~ . + poly(time.on.Twitter.s, 2))
f1.5alt <- update(f1.2m,  . ~ . + time.on.Twitter.s + time.on.Twitter.s2)

etable(f1.5, f1.5.rawpoly, f1.5.orgpoly, fitstat = ~ . + ll + aic) # Second order polynomials are n.s

# Model 1.7: Continuous time on Twitter as random slope
f1.7 <- feglm(response ~ 
                twitter.valence.90d + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d	+ 
                i(sentiment, ref = "twitter.neutral") + 
                weekend |
                agencyname[time.on.Twitter.s],
              family = "logit",
              data = data.removed)

etable(f1.5, f1.7, fitstat = ~ . + ll + aic)  # Big importoment in terms of AIC/LL, but slightly worse BIC

# Model 1.8: Continuous time on Twitter + poly as random slope
f1.8 <- feglm(response ~ 
                twitter.valence.90d + 
                media.valence.90d +
                media.count.90d.log +
                short +
                qm.comment +
                attachment +
                conversations.log +
                information.90d	+ 
                i(sentiment, ref = "twitter.neutral") + 
                weekend |
                agencyname[time.on.Twitter.s, time.on.Twitter.s2],
              family = "logit",
              data = data.removed)

etable(f1.7, f1.8, fitstat = ~ . + ll + aic)  # Only one varying slope seems to be most parsimonious

# Select model with best fit
etable(f1.3, f1.7, fitstat = ~ . + ll + aic) # Again, varying slope outperforms agency FE + year FE

# Try different lags
f1.7month <- update(f1.7, . ~ . - 
                      twitter.valence.90d - 
                      media.valence.90d -
                      media.count.90d.log +
                      twitter.valence.30d + 
                      media.valence.30d +
                      media.count.30d.log)

f1.7year <- update(f1.7, . ~ . - 
                      twitter.valence.90d - 
                      media.valence.90d -
                      media.count.90d.log +
                      twitter.valence.365d + 
                      media.valence.365d +
                      media.count.365d.log)

obs.removed2 <- f1.7year$obs_selection$obsRemoved
data.removed2 <- data.removed[(obs.removed2),]

f1.7m <- update(f1.7, . ~ . , data = data.removed2)
f1.7monthm <- update(f1.7month, . ~ . , data = data.removed2)

etable(f1.7m, f1.7monthm, f1.7year, fitstat = ~ . + ll + aic) # Year seems best

# Model 1.8: Adding controls with some missingness
f1.8 <- update(f1.7year, . ~ . +
                mention +
                qm.agency,
                data = response.data) 

etable(f1.8, fitstat = ~ . + ll + aic)

# Clustering

# Model 1.8: conversation as cluster
f1.8.conversation <- update(f1.8, cluster = ~ conversation_id)
f1.8.agencyyear <- update(f1.8, cluster = ~ agencyname^year)

etable(f1.8, f1.8.conversation, f1.8.agencyyear, fitstat = ~ . + ll + aic)

# Model evaluation

# A look at the fixed effects
f1.8.plot <- fixef(f1.8)
plot(f1.8.plot)

# Quick export
# Add info on varying slope

models <- list(
  "Model 1.1 (cluster = agency)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log |
                    agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit")),
  "Model 2.1 (cluster = conversation)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log | 
                    agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~conversation_id),
  "Model 3.1 (cluster = agency-year)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log |
                    agencyname + agencyname[time.on.Twitter.s], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~agencyname^year),
  "Model 1.2 (cluster = agency)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log +
                      short + qm.comment + attachment + conversations.log + information.90d + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency | 
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit")),
  "Model 2.2 (cluster = conversation)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log + 
                      short + qm.comment + attachment + conversations.log + information.90d + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency |
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~conversation_id),
  "Model 3.2 (cluster = agency-year)" = feglm(fml = response ~ twitter.valence.365d + media.valence.365d + media.count.365d.log +
                      short + qm.comment + attachment + conversations.log + information.90d + i(sentiment, ref = "twitter.neutral") + weekend + mention + qm.agency |
                      agencyname + agencyname[[time.on.Twitter.s]], 
                    data = response.data, family = binomial(link = "logit"), cluster = ~agencyname^year))


modelsummary(models,
             estimate = "{estimate} ({std.error})",
             statistic = "p = {p.value}") # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

# ======================================================
#           # Robustness check: GLMMadaptive
# ======================================================

# Construct dataset with same observation
obs.removed3 <- f1.8$obs_selection$obsRemoved
data.removed3 <- response.data[(obs.removed3),]

data.demean <- cbind(
  data.removed3,
  datawizard::demean(data.removed3,
                     select = c("twitter.valence.365d", "media.valence.365d", "media.count.365d.log",
                                "short", "time.on.Twitter.s", "qm.comment", "attachment", "conversations.log", "sentiment.factor", "information.90d", "weekend", "mention", "qm.agency"), group = "agencyname"))

# ======================================================
#           # Random effects: GLMMadaptive
# ======================================================

r1.8.year <- mixed_model(fixed = response ~ twitter.valence.365d_within + media.valence.365d_within + media.count.365d.log_within + 
                           short_within + qm.comment_within + attachment_within + conversations.log_within + information.90d_within + sentiment.factor_twitter.criticism_within + sentiment.factor_twitter.praise_within + weekend_within + mention_within  + qm.agency_within + time.on.Twitter.s_within,
                    random = ~ time.on.Twitter.s_within | agencyname, 
                    data = data.demean,
                    family = binomial())

summary(g1.8.year)
