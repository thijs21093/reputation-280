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
library(panelr)

load("./data/response_tweet2.Rda")


cubic.root <- function(x) {
  sign(x) * abs(x)^(1/3)
}

# ======================================================
#           Centering, transformation & description: cross-sectional
# ======================================================
# Transformation
# ======================================================
response.data <- response.tweet %>%
  filter(agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(
         
         # Misc.
        # conversations.log = log2(conversations),
         sentiment.factor = as.factor(sentiment),
         agencyyear = paste0(agencyname, "-", year),
         
         # Log
         media.count.1w.log = log2(media.count.1w + 1),
         twitter.index.1w.cr = cubic.root(twitter.index.1w),
         comments.1w.log = log(comments.1w + 1),
         
         # Scale time on Twitter
         time.on.Twitter.s = time.on.Twitter/365,
         time.on.Twitter.s2 = time.on.Twitter.s^2)
  
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
#           Cross-sectional fixed effect
# ======================================================

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
              agencyname,
              family = "logit",
              data = response.data) 

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
                agencyname^year,
              family = "logit",
              data = response.data) 

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
       fitstat = ~ . + ll + aic) # Fit statistics

# Which model has the best fit indices?
# Model 1.3

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
