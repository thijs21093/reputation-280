# Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(sjmisc)
library(kableExtra)
library(misty)
library(multcomp)
library(GLMMadaptive)
library(effects)
library(sjPlot)

load("./data/data_day.Rda")

# ======================================================
#           Centering, transformation & description
# ======================================================

conversation <- data.merge %>% group_by(conversation_id) %>%
  summarise(conversations = length(unique(conversation_id)))

data.merge <- data.merge %>% full_join(conversation)

# To do: move to other script

# Centering & transformation
# ======================================================
reg <- data.merge %>% mutate(
                             time.year = (time/365),
                             response.num = case_when(
                               response == "response" ~ 1,
                               response == "no.response" ~ 0),
                             sentiment.num = case_when(
                               sentiment == "positive" ~ 1,
                               sentiment == "neutral" ~ 0,
                               sentiment == "negative" ~ -1),
                             positive_tweet = case_when(
                               sentiment == "positive" ~ 1,
                               TRUE ~ 0),
                             neutral_tweet = case_when(
                               sentiment == "neutral" ~ 1,
                               TRUE ~ 0),
                             negative_tweet = case_when(
                               sentiment == "negative" ~ 1,
                               TRUE ~ 0), 
  length.cgm = center(length), # CGM
  question.mark.cgm = center(question.mark), # CGM
  valence.3m.cwc = center(valence.3m, type = "CWC", cluster = data.merge$agencyname), # CWC
  media_count.3m.cwc = center(media_count.3m, type = "CWC", cluster = data.merge$agencyname), # CWC
  positive_tweet.cwc = center(positive_tweet, type = "CWC", cluster = data.merge$agencyname),
  neutral_tweet.cwc = center(neutral_tweet, type = "CWC", cluster = data.merge$agencyname),
  negative_tweet.cwc = center(negative_tweet, type = "CWC", cluster = data.merge$agencyname), 
  sentiment.num.cwc = center(sentiment.num, type = "CWC", cluster = data.merge$agencyname))

# TO DO: move code chunk to other script
# TO DO: add attachement do dataset
# TO DO: check no repsonses for frontex
# TO DO: check replies that don't start with "@"



x <- c("response.num",
       "time.year",
       "negative_tweet.cwc",
       "neutral_tweet.cwc",
       "positive_tweet.cwc",
       "information.1w.lcwc",
       "media_count.3m.cwc",
       "question.mark.cgm",
       "length.cgm")

var.table <- reg %>%
  dplyr::select(c(response.num,
                  time.year,
                  negative_tweet.cwc,
                  neutral_tweet.cwc,
                  positive_tweet.cwc,
                  information.1w.lcwc,
                  media_count.3m.cwc,
                  question.mark.cgm,
                  length.cgm)) %>%
  descr() %>% 
  data.frame() %>% 
  dplyr::select(-type,
                -var,
                -NA.prc,
                -se,
                -trimmed,
                -iqr) %>%
  mutate_at(vars(c("mean","sd","md")),funs(round(.,3))) %>%
  slice(match(x, label)) %>% dplyr::select(-label)

var.label <- c("Response",
               "Time (years)",
               "Negative tweet (CWC)",
               "Neutral tweet (CWC)",
               "Positive tweet (CWC)",
               "Media count, prev 3 months (CWC)",
               "Question mark (GMC)",
               "Length (GMC)")

row.names(var.table) <- var.label
var.table %>%
  kable(col.names = c("n",
                      "Mean",
                      "SD",
                      "Median",
                      "Range",
                      "Skew")) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")

# Percentages for each agency
reg %>%
group_by(agencyname) %>%
  summarize(total = n(),
            prop = sum(response.num == 1) / n()) %>%
  arrange(desc(prop)) %>%
  kable(col.names = c("Agency",
                      "Total",
                      "Response (%)"),
        digits = 2) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")


# ======================================================
#           GLMM
# ======================================================

# Testing for random effect
# ======================================================
m1.0 <- mixed_model(fixed = response.num ~ time.year, # As per: https://drizopoulos.github.io/GLMMadaptive/articles/GLMMadaptive.html
                    random = ~ 1 | agencyname,
                    data = reg,
                   family = binomial())

summary(m1.0)

m1.1 <- glm(response.num ~ time.year, data = reg, family = binomial())

summary(m1.1)

anova(m1.0, m1.1) # A highly significant p-value suggests that there are correlations in the data that cannot be ignored.
                  # Boundary test would be better

m1.2 <- mixed_model(fixed = response.num ~ time.year,
                    random = ~ time.year || agencyname, # || means that the covariance between the random intercepts and random slopes is zero.
                    data = reg,
                    family = binomial())

summary(m1.2)

anova(m1.0, m1.2) # Significant p-value suggests that we need the random slopes term.


m1.3 <- mixed_model(fixed = response.num ~ time.year,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m1.3)

anova(m1.2, m1.3) # Non-significant p-value suggests that the covariance between the two random effects terms is not statistically different from zero.

# Stepwise regression
# ======================================================

# Salience
m2.0 <- mixed_model(fixed = response.num ~ 
                    audience_threat.1d.cwc +
                    audience_count.1d.lcwc +
                    media_count.3m.cwc +
                    time.year,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m2.0)

# Valence
m2.1 <- mixed_model(fixed = response.num ~ 
                      valence.3m.cwc +
                      sentiment.num.cwc +
                      time.year,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m2.1)

m2.2 <- mixed_model(fixed = response.num ~ 
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      time.year,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m2.2)

# L1 controls
m2.3 <- mixed_model(fixed = response.num ~ 
                    question.mark.cgm +
                    length.cgm +
                    information.1w.lcwc +
                    time.year,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m2.3)

# All vars

reg.na <- reg %>%
  select(response.num, 
  audience_threat.1d.cwc,
  audience_count.1d.lcwc,
  media_count.3m.cwc,
  valence.3m.cwc,
  negative_tweet.cwc,
  positive_tweet.cwc,
  question.mark.cgm,
  length.cgm,
  information.1w.lcwc,
  time.year,
  agencyname) %>%
  drop_na()

m2.4 <- mixed_model(fixed = response.num ~ 
                      audience_threat.1d.cwc +
                      audience_count.1d.lcwc +
                      media_count.3m.cwc +
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      question.mark.cgm +
                      length.cgm +
                      information.1w.lcwc +
                      time.year,
                    random = ~ time.year | agencyname,
                    data = reg.na,
                    family = binomial())

summary(m2.4)

ranef(m2.4)

# Interaction effect
# ======================================================

# Valence X Salience
m3.0 <- mixed_model(fixed = response.num ~ 
                      audience_threat.1d.cwc +
                      audience_count.1d.lcwc +
                      media_count.3m.cwc +
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      question.mark.cgm +
                      length.cgm +
                      information.1w.lcwc +
                      time.year +
                      valence.3m.cwc:audience_threat.1d.cwc,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m3.0)

plot(Effect(c("audience_threat.1d.cwc", "valence.3m.cwc"), m3.0),
     rug = FALSE,
     xlab = "Audience threat",
     ylab = "P(repsponse)")

m3.1 <- mixed_model(fixed = response.num ~ 
                      audience_threat.1d.cwc +
                      audience_count.1d.lcwc +
                      media_count.3m.cwc +
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      question.mark.cgm +
                      length.cgm +
                      information.1w.lcwc +
                      time.year +
                      valence.3m.cwc:audience_count.1d.lcwc,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m3.1)

plot(Effect(c("audience_count.1d.lcwc", "valence.3m.cwc"), m3.1),
     rug = FALSE,
     xlab = "Audience count",
     ylab = "P(repsponse)")

m3.2 <- mixed_model(fixed = response.num ~
                      audience_threat.1d.cwc +
                      audience_count.1d.lcwc +
                      media_count.3m.cwc +
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      question.mark.cgm +
                      length.cgm +
                      information.1w.lcwc +
                      time.year +
                      valence.3m.cwc:media_count.3m.cwc,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m3.2)

plot(Effect(c("media_count.3m.cwc", "valence.3m.cwc"), m3.2),
     rug = FALSE,
     xlab = "Media count",
     ylab = "P(repsponse)")

m3.3 <- mixed_model(fixed = response.num ~
                      audience_threat.1d.cwc +
                      audience_count.1d.lcwc +
                      media_count.3m.cwc +
                      valence.3m.cwc +
                      negative_tweet.cwc + 
                      positive_tweet.cwc +
                      question.mark.cgm +
                      length.cgm +
                      information.1w.lcwc +
                      time.year +
                      hearing.3m.cwc +
                      valence.3m.cwc:hearing.3m.cwc,
                    random = ~ time.year | agencyname,
                    data = reg,
                    family = binomial())
summary(m3.3)

plot(Effect(c("hearing.3m.cwc", "valence.3m.cwc"), m3.3),
     rug = FALSE,
     xlab = "Hearing",
     ylab = "P(repsponse)")

# Exporting
tab_model(m3.0, m3.1, m3.2, m3.3,
          show.ci = F)

descr(reg$media_count.3m.cwc)
