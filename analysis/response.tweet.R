# Library
library(dplyr)
library(glmmTMB)
library(misty)
library(lubridate)
library(tidyr)
library(parameters)
library(MASS)
library(GLMMadaptive)
library(kableExtra)
library(sjmisc)
library(tidyselect)
library(performance)
library(sjPlot)
library(fixest)
library(lmtest)
library(sandwich)
library(foreign)
library(broom)
library(modelsummary)
library(flextable)
library(misty)
library(ggplot2)
library(marginaleffects)

# Set time zone
Sys.time()
Sys.timezone()

load("./data/response_tweet3.Rda")

# ======================================================
#           Centering, transformation & description
# ======================================================

# Which agencies have been most responsive?
# ======================================================
response.tweet %>%
  group_by(agencyname) %>%
  summarize(total = n(),
            response = sum(response == 1),
            prop = response / total * 100,
            response.comment = sum(response.comment == 1),
            prop.comment = response.comment / total * 100) %>%
  arrange(desc(prop)) %>%
  kable(col.names = c("Agency",
                      "No. of user comments",
                      "No. of responses",
                      "Response rate (%)",
                      "No. of responses (excl. retweets)",
                      "Response rate (%, excl. retweets)" ),
        digits = 1) %>% 
  kable_styling(bootstrap_options = c("condensed"),
                full_width = F,
                position = "left")

# Transformation
# ======================================================
# Remove tweets with no sentiment score
response.tweet2 <- response.tweet %>%
  filter(agencyname != "EIOPA") %>% # Only zeros
  tidyr::drop_na(weighted.score) %>% # No sentiment produced for some tweets
  
  mutate(
  # Log
  likes.log = log(like_count + 1),

  # Factor  
  attachment = as.factor(attachment),
  year.factor = as.factor(year),
  agency.factor = as.factor(agencyname),
  int.trad = weighted.score*anomaly.cencor,
  id = row_number(),
  id.n = as.numeric(agency.factor),
  weighted.score.c = center(weighted.score),
  anomaly.cencor.c = center(anomaly.cencor),
  weighted.score2 = weighted.score.c^2,
  sentiment.cat = case_when(
    score > 0.3 ~ "pos",
    score >= -0.3 & score <= 0.3 ~ "neu",
    score < -0.3 ~ "neg"),
  sentiment.cat = as.factor(sentiment.cat)) %>%
  group_by(agencyname) %>%
  mutate(tid = row_number())

# Relevel factor
response.tweet2 <- within(response.tweet2, attachment <- relevel(attachment, ref = "no media"))
response.tweet2 <- within(response.tweet2, sentiment.cat <- relevel(sentiment.cat, ref = "neu"))


##############################
# Original models PMRC (but with new attachment variable)
##############################
fe1 <- fixest::feglm(response ~ weighted.score +
                        anomaly.cencor | 
                        agencyname,
                        cluster = "agencyname",
                        family = "logit",
                        data = response.tweet2) 
summary(fe1)
etable(fe1, fitstat = ~ . + ll + aic)
tidy(fe1)

fe2 <- feglm(response ~ weighted.score +
               anomaly.cencor +
               weighted.score*anomaly.cencor | 
               agencyname,
             cluster = "agencyname",
             family = "logit",
             data = response.tweet2) 

summary(fe2)
etable(fe1, fe2, fitstat = ~ . + ll + aic)  

fe3 <- fixest::feglm(response ~ weighted.score +
               anomaly.cencor +
               weighted.score*anomaly.cencor +
               conversations +
               likes.log +
               short +
               qm.comment +
               attachment +
               same.message  +  
               uncivil.tweet | 
               agencyname,
             cluster = "agencyname",
             family = "logit",
             data = response.tweet2) 

summary(fe3)
etable(fe2, fe3, fitstat = ~ . + ll + aic)  

fe4 <- feglm(response ~ weighted.score +
               anomaly.cencor +
               weighted.score*anomaly.cencor +
               conversations +
               likes.log +
               short +
               qm.comment +
               attachment +
               same.message  +
               uncivil.tweet +
               weekend +
               office.hours | 
               agencyname + year,
             cluster = "agencyname",
             family = "logit",
             data = response.tweet2) 


summary(fe4)
etable(fe3, fe4, fitstat = ~ . + ll + aic)

etable(fe1, fe2, fe3, fe4, fitstat = ~ . + ll + aic)


models <- list(
  "Model 1" = fe1,
  "Model 2" = fe2,
  "Model 3" = fe3,
  "Model 4" = fe4)

modelsummary(models,
             fmt = 2,
             coef_rename = c(
               "weighted.score" = "Sentiment",
               "anomaly.cencor" = "Media storm",
               "weighted.score:anomaly.cencor" = "Sentiment * Media storm", 
               "conversations" = "Thread size", 
               "likes.log" = "Likes (log)",
               "short" = "Short",
               "attachment" = "Attachment", 
               "same.message"	=  "Same message",
               "qm.comment" = "Question mark", 
               "uncivil.tweet" = "Impolite",
               "weekend" = "Weekend",
               "office.hours" = "Office hours"),
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             output = "feglm-stars.docx",
             gof_omit = 'R2|Errors'
             ) # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html


etable(models, fitstat = ~ . + ll + aic)

##############################
# New models
##############################

# Broad
##############################
fe1.b <- fixest::feglm(response ~ 
                       anomaly.cencor +
                       score + 
                       Flesch.adj | 
                       agencyname,
                     cluster = "agencyname",
                     family = "logit",
                     data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe1.b)
etable(fe1.b, fitstat = ~ . + ll + aic)

fe2.b <- fixest::feglm(response ~ 
                       score*anomaly.cencor +
                       Flesch.adj | 
                       agencyname,
                           cluster = "agencyname",
                           family = "logit",
                           data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe2.b)
etable(fe2.b, fitstat = ~ . + ll + aic)
plot_model(fe2.b, "int", mdrt.values	= "meansd")

fe3.b <- fixest::feglm(response ~ 
                       score +
                       Flesch.adj*anomaly.cencor | 
                       agencyname,
                           cluster = "agencyname",
                           family = "logit",
                           data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe3.b)
etable(fe3.b, fitstat = ~ . + ll + aic)
plot_model(fe3.b, "int", mdrt.values	= "meansd")

fe4.b <- fixest::feglm(response ~ 
                             anomaly.cencor +
                             Flesch.adj*score | 
                             agencyname,
                           cluster = "agencyname",
                           family = "logit",
                           data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe4.b)
etable(fe4.b, fitstat = ~ . + ll + aic)
plot_model(fe4.b, "int", mdrt.values	= "meansd")

etable(fe2.b, fe3.b, fe4.b, fitstat = ~ . + ll + aic)


# Adding controls (broad)
##############################

# Controls, no interaction
fe1.bint <- fixest::feglm(response ~  
                            score +
                            anomaly.cencor +
                            Flesch.adj +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe1.bint)
etable(fe1.bint, fitstat = ~ . + ll + aic)

# Sentiment * Media storm
fe2.bint <- fixest::feglm(response ~ 
                             score*anomaly.cencor +
                             Flesch.adj +
                             short +
                             emoji +
                             hashtag +
                             qm.comment +
                             attachment +
                             same.message  +
                             uncivil.tweet +
                             weekend +
                             office.hours | 
                             agencyname + year,
                           cluster = "agencyname",
                           family = "logit",
                           data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe2.bint)

etable(fe2.bint, fitstat = ~ . + ll + aic)

plot.fe2 <- plot_model(fe2.bint, "int", mdrt.values	= "meansd")
plot.fe2 + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Tweet complexity * Media storm
fe3.bint <- fixest::feglm(response ~ 
                                 score +
                                 Flesch.adj*anomaly.cencor +
                                 short +
                                 emoji +
                                 hashtag +
                                 qm.comment +
                                 attachment +
                                 same.message  +
                                 uncivil.tweet +
                                 weekend +
                                 office.hours | 
                                 agencyname + year,
                               cluster = "agencyname",
                               family = "logit",
                               data = response.tweet2 %>% filter(Flesch.adj >= 0))


tidy(fe3.bint)

etable(fe3.bint, fitstat = ~ . + ll + aic)

plot.fe3 <- plot_model(fe3.bint, "int", mdrt.values	= "meansd")
plot.fe3 + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Tweet complexity * sentiment
fe4.bint <- fixest::feglm(response ~ 
                                 anomaly.cencor +
                                 Flesch.adj*score +
                                 short +
                                 emoji +
                                 hashtag +
                                 qm.comment +
                                 attachment +
                                 same.message  +
                                 uncivil.tweet +
                                 weekend +
                                 office.hours | 
                                 agencyname + year,
                               cluster = "agencyname",
                               family = "logit",
                               data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe4.bint)

etable(fe4.bint, fitstat = ~ . + ll + aic)

plot.fe4 <- plot_model(fe4.bint, "int", mdrt.values	= "meansd")
plot.fe4 + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Results models with interaction and controls
etable(fe2.bint, fe3.bint,fe4.bint, fitstat = ~ . + ll + aic)

# All interactions together
fe5.bint <- fixest::feglm(response ~ 
                            Flesch.adj*score +
                            Flesch.adj*anomaly.cencor +
                            score*anomaly.cencor +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe5.bint)

etable(fe5.bint, fitstat = ~ . + ll + aic)

plot_model(fe5.bint, "int", mdrt.values	= "meansd")[[1]]
plot_model(fe5.bint, "int", mdrt.values	= "meansd")[[2]]
plot_model(fe5.bint, "int", mdrt.values	= "meansd")[[3]]

# plotting models
models.broad <- list(
  "Model 1" = fe2.bint,
  "Model 2" = fe2.bint,
  "Model 3" = fe3.bint,
  "Model 4" = fe4.bint,
  "Model 5" = fe5.bint)

modelsummary(models.broad,
             fmt = 2,
             coef_rename = c(
               "score" = "Sentiment",
               "anomaly.cencor" = "Media storm",
               "Flesch.adj" = "Tweet complexity",
               "score:anomaly.cencor" = "Sentiment * Media storm",
               "Flesch.adj:anomaly.cencor" = "Tweet complexity * Media storm", 
               "Flesch.adj:score" = "Tweet complexity * Sentiment", 
               "emoji" = "Emoji",
               "hashtag" = "hashtag",
               "short" = "Short",
               "attachmentlink" = "Link", 
               "attachmentvisual" = "Visual",
               "same.message"	=  "Same message",
               "qm.comment" = "Question mark", 
               "uncivil.tweet" = "Impolite",
               "weekend" = "Weekend",
               "office.hours" = "Office hours"),
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             output = "feglm-comment-broad.docx",
             gof_omit = 'R2|Errors|RMSE') # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

##############################
# Alternatives
##############################

# Polynomial
response.tweet3 <- response.tweet2  %>%
  filter(Flesch.adj >= 0) %>%
  mutate(!!!as.data.frame(poly(x = .$Flesch.adj, degree = 2))) %>%
  rename("poly1" = `1`,
         "poly2" = `2`)

alt1 <- fixest::feglm(response ~ 
                            anomaly.cencor +
                            poly1*score +
                            score*poly2 +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet3 %>% filter(Flesch.adj >= 0))

etable(alt1, fitstat = ~ . + ll + aic)

plot_model(alt1, "int", mdrt.values	= "meansd")[[1]]
plot_model(alt1, "int", mdrt.values	= "meansd")[[2]]

# Note: Sign. but not different substantially

# Sentiment as categorical variable
alt2 <- fixest::feglm(response ~ 
                              sentiment.cat * anomaly.cencor +
                              Flesch.adj +
                              short +
                              emoji +
                              hashtag +
                              qm.comment +
                              attachment +
                              same.message  +
                              uncivil.tweet +
                              weekend +
                              office.hours | 
                              agencyname + year,
                            cluster = "agencyname",
                            family = "logit",
                            data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(alt2)
etable(alt2, fitstat = ~ . + ll + aic)

# Three-way
alt3 <- fixest::feglm(response ~ 
                            Flesch.adj*score*anomaly.cencor +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))


tidy(alt3)
etable(alt3, fitstat = ~ . + ll + aic)


# Narrow
##############################
fe1.n <- fixest::feglm(response.comment ~ 
                         anomaly.cencor +
                         score + 
                         Flesch.adj | 
                         agencyname,
                       cluster = "agencyname",
                       family = "logit",
                       data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe1.n)
etable(fe1.n, fe1.b, fitstat = ~ . + ll + aic)

fe2.n <- fixest::feglm(response.comment ~ 
                         score*anomaly.cencor +
                         Flesch.adj | 
                         agencyname,
                       cluster = "agencyname",
                       family = "logit",
                       data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe2.n)
etable(fe2.n, fe2.b, fitstat = ~ . + ll + aic)
plot_model(fe2.n, "int", mdrt.values	= "meansd")

fe3.n <- fixest::feglm(response.comment ~ 
                         score +
                         Flesch.adj*anomaly.cencor | 
                         agencyname,
                       cluster = "agencyname",
                       family = "logit",
                       data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe3.n)
etable(fe3.n, fe3.b, fitstat = ~ . + ll + aic)
plot_model(fe3.n, "int", mdrt.values	= "meansd")

fe4.n <- fixest::feglm(response.comment ~ 
                         anomaly.cencor +
                         Flesch.adj*score | 
                         agencyname,
                       cluster = "agencyname",
                       family = "logit",
                       data = response.tweet2 %>% filter(Flesch.adj >= 0)) 

tidy(fe4.n)
etable(fe4.n, fe4.b, fitstat = ~ . + ll + aic)
plot_model(fe4.n, "int", mdrt.values	= "meansd")

etable(fe2.n, fe3.n, fe4.n, fitstat = ~ . + ll + aic)


# Adding controls (narrow)
##############################

# Controls, no interaction
fe1.nint <- fixest::feglm(response.comment ~  
                            score +
                            anomaly.cencor +
                            Flesch.adj +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe1.nint)
etable(fe1.nint, fe1.bint, fitstat = ~ . + ll + aic)

# Sentiment * Media storm
fe2.nint <- fixest::feglm(response.comment ~ 
                            score*anomaly.cencor +
                            Flesch.adj +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe2.nint)

etable(fe2.nint, fe2.bint, fitstat = ~ . + ll + aic)

plot.fe2n <- plot_model(fe2.nint, "int", mdrt.values	= "meansd")
plot.fe2n + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Tweet complexity * Media storm
fe3.nint <- fixest::feglm(response.comment ~ 
                            score +
                            Flesch.adj*anomaly.cencor +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))


tidy(fe3.nint)

etable(fe3.nint, fe3.bint, fitstat = ~ . + ll + aic)

plot.fe3n <- plot_model(fe3.nint, "int", mdrt.values	= "meansd")
plot.fe3n + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Tweet complexity * sentiment
fe4.nint <- fixest::feglm(response.comment ~ 
                            anomaly.cencor +
                            Flesch.adj*score +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe4.nint)

etable(fe4.nint, fe4.bint, fitstat = ~ . + ll + aic)

plot.fe4n <- plot_model(fe4.nint, "int", mdrt.values	= "meansd")
plot.fe4n + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + theme_sjplot()

# Results models with interaction and controls
etable(fe2.nint, fe3.nint,fe4.nint, fitstat = ~ . + ll + aic)

# All interactions together
fe5.nint <- fixest::feglm(response.comment ~ 
                            Flesch.adj*score +
                            Flesch.adj*anomaly.cencor +
                            score*anomaly.cencor +
                            short +
                            emoji +
                            hashtag +
                            qm.comment +
                            attachment +
                            same.message  +
                            uncivil.tweet +
                            weekend +
                            office.hours | 
                            agencyname + year,
                          cluster = "agencyname",
                          family = "logit",
                          data = response.tweet2 %>% filter(Flesch.adj >= 0))

tidy(fe5.nint)

etable(fe5.nint, fitstat = ~ . + ll + aic)

plot_model(fe5.nint, "int", mdrt.values	= "meansd")[[1]]
plot_model(fe5.nint, "int", mdrt.values	= "meansd")[[2]]
plot_model(fe5.nint, "int", mdrt.values	= "meansd")[[3]]

# plotting models
models.broad <- list(
  "Model 1" = fe2.nint,
  "Model 2" = fe2.nint,
  "Model 3" = fe3.nint,
  "Model 4" = fe4.nint,
  "Model 5" = fe5.nint)

modelsummary(models.broad,
             fmt = 2,
             coef_rename = c(
               "score" = "Sentiment",
               "anomaly.cencor" = "Media storm",
               "Flesch.adj" = "Tweet complexity",
               "score:anomaly.cencor" = "Sentiment * Media storm",
               "Flesch.adj:anomaly.cencor" = "Tweet complexity * Media storm", 
               "Flesch.adj:score" = "Tweet complexity * Sentiment", 
               "emoji" = "Emoji",
               "hashtag" = "hashtag",
               "short" = "Short",
               "attachmentlink" = "Link", 
               "attachmentvisual" = "Visual",
               "same.message"	=  "Same message",
               "qm.comment" = "Question mark", 
               "uncivil.tweet" = "Impolite",
               "weekend" = "Weekend",
               "office.hours" = "Office hours"),
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             output = "feglm-comment-broad.docx",
             gof_omit = 'R2|Errors|RMSE') # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html



