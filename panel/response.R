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
library(lme4)
library(multilevel)


# TO DO: move code chunk to other script
# TO DO: add attachement do dataset
# TO DO: check no repsonses for frontex
# TO DO: check replies that don't start with "@"

load("./data/data_day.Rda")

# ======================================================
#           Centering, transformation & description
# ======================================================

conversation <- data.merge %>% group_by(conversation_id) %>%
  summarise(conversations = length(conversation_id))

data.merge <- data.merge %>% full_join(conversation)

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
  conversations.cwc = center(conversations, type = "CWC", cluster = data.merge$agencyname), # CGM
  positive_tweet.cgm = center(positive_tweet), # CGM
  neutral_tweet.cgm = center(neutral_tweet), # CGM
  negative_tweet.cgm = center(negative_tweet), # CGM
  question.mark.cgm = center(question.mark), # CGM
  valence.3m.cwc = center(valence.3m, type = "CWC", cluster = data.merge$agencyname), # CWC
  media_count.3m.cwc = center(media_count.3m, type = "CWC", cluster = data.merge$agencyname)) # CWC



x <- c("response.num",
       "time.year",
       "negative_tweet.cgm",
       "neutral_tweet.cgm",
       "positive_tweet.cgm",
       "valence.3m.cwc",
       "media_count.3m.cwc",
       "question.mark.cgm",
       "length.cgm",
       "conversations.cwc")

var.table <- reg %>%
  dplyr::select(c(response.num,
                  time.year,
                  negative_tweet.cgm,
                  neutral_tweet.cgm,
                  positive_tweet.cgm,
                  valence.3m.cwc,
                  media_count.3m.cwc,
                  question.mark.cgm,
                  length.cgm,
                  conversations.cwc)) %>%
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
               "Negative tweet (GMC)",
               "Neutral tweet (GMC)",
               "Positive tweet (GMC)",
               "Valence, prev. 3 months (CWC)",
               "Media count, prev 3 months (CWC)",
               "Question mark (GMC)",
               "Length (GMC)",
               "Length of conservation")

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

# Empty model
l1.0 <- glmer(response.num ~ 1 +
                    (1 | agencyname / conversation_id),
                    na.action = na.omit,
                    data = reg,
                    family = binomial(link = "logit"))

summary(l1.0)
performance::icc(l1.1, by_group = TRUE) # Variance by group


# Adding time as fixed effect
l1.1 <- glmer(response.num ~ time.year +
                (1 | agencyname) +
                (1 | agencyname:conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.1)
anova(l1.0, l1.1)

# Adding time as random effect
l1.2 <- glmer(response.num ~ time.year +
                (time.year | agencyname) +
                (1 | agencyname:conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.2)
anova(l1.1, l1.2)

# Note: Does not converge!

# Adding main IVs
l1.3 <- glmer(response.num ~ time.year +
              valence.3m.cwc +
              media_count.3m.cwc +
              (1 | agencyname / conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.3)

# Adding interaction effect
l1.4 <- glmer(response.num ~ time.year +
                valence.3m.cwc*media_count.3m.cwc +
                (1 | agencyname / conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.4)

plot(Effect(c("media_count.3m.cwc", "valence.3m.cwc"), l1.2),
     rug = FALSE,
     xlab = "Media salience",
     ylab = "P(repsponse)")

# Adding L1 control variables
l1.5 <- glmer(response.num ~ time.year +
                valence.3m.cwc*media_count.3m.cwc +
                sentiment +
                question.mark.cgm +
                length.cgm +
                (1 | agencyname / conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.5)

# Adding L2 control variables
l1.6 <- glmer(response.num ~ time.year +
                sentiment +
                question.mark.cgm +
                length.cgm +
                conversations.cwc +
                valence.3m.cwc*media_count.3m.cwc +
                (1 | agencyname / conversation_id),
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l1.6)

# Adding L3 control variables
## Not yet included in dataset




# Exporting
tab_model(m3.0, m3.1, m3.2, m3.3,
          show.ci = F)

descr(reg$media_count.3m.cwc)
