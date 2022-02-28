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
library(lmtest)
library(sandwich)
library(merDeriv)
library(lfe)
library(fixest)
library(modelsummary)

# TO DO: move code chunk to other script
# TO DO: add attachement do dataset
# TO DO: check replies that don't start with "@"

load("./data/data_day.Rda")

# ======================================================
#           Centering, transformation & description
# ======================================================

conversation <- data.merge %>% group_by(conversation_id) %>%
  summarise(conversations = length(conversation_id))

data.merge <- data.merge %>% full_join(conversation, by = "conversation_id")

# Centering & transformation: cross-sectional
# ======================================================
reg <- data.merge %>% mutate(year =  format(as.POSIXct(day), format="%Y"),
                             weekday = weekdays(as.Date(day)), # weekend versus weekday
                             time.1000 = (time/1000),
                             weekend = case_when(
                                weekday == "zaterdag" ~ "weekend",
                                weekday == "zondag" ~ "weekend",
                                TRUE ~ "weekday"),
                             response.num = case_when(
                               response == "response" ~ 1,
                               response == "no.response" ~ 0)) %>%
  drop_na(valence.3m, media_count.3m) %>%
mutate(length.s = scale(length),
      valence.3m.s  = scale(valence.3m),
      media_count.3m.s  = scale(media_count.3m), 
      conversations.s = scale(conversations))

x <- c("response.num",
       "time",
       "valence.3m",
       "media_count.3m",
       "question.mark",
       "length",
       "conversations")

var.table <- reg %>%
  dplyr::select(c(response.num,
                  time,
                  valence.3m,
                  media_count.3m,
                  question.mark,
                  length,
                  conversations)) %>%
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
               "Time (days)",
               "Valence, prev. 3 months",
               "Media count, prev 3 months",
               "Question mark",
               "Length",
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

# Centering & transformation: panel
# ======================================================



# How often do agencies repond?
# ======================================================
reg %>%
group_by(agencyname) %>%
  summarize(total = n(),
            response = sum(response.num == 1),
            prop = sum(response.num == 1) / n()) %>%
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

# NOTE: for model comparison, check if models have the same number of observations
# To do: Add other variables of interest to models. THis includes time since joining date

# Model 1.0: Main IVs
f1.0 <- feglm(response.num ~ 
                valence.3m + 
                media_count.3m,
              family = "logit",
              data = reg) 

summary(f1.0)

# Model 1.1: Adding controls except time since joining Twitter
f1.1 <- update(f1.0, . ~ . +
                 i(sentiment, ref = "neutral") +
                 i(weekend, ref = "weekday") + 
                 length + 
                 question.mark + 
                 conversations,
              family = "logit",
              data = reg) 

summary(f1.1)

# Model 1.2: adding time since joining data
# f1.2 <- update(f1.1 , . ~ . + )

summary(f1.2)

# Model 1.3: agency fixed effects

f1.3 <- feglm(response.num ~ 
                valence.3m + 
                media_count.3m +
                i(sentiment, ref = "neutral") +
                i(weekend, ref = "weekday") + 
                length + 
                question.mark + 
                conversations |
                agencyname,
                family = "logit",
                data = reg)
summary(f1.3)

# Model 1.4: time as FE effect
f1.4 <- feglm(response.num ~ 
                valence.3m + 
                media_count.3m +
                i(sentiment, ref = "neutral") +
                i(weekend, ref = "weekday") + 
                length + 
                question.mark + 
                conversations |
                agencyname + year,
              family = "logit",
              data = reg)

summary(f1.4)

# Model 1.5: time x agency interaction as FE effect
f1.5 <- feglm(response.num ~ 
                valence.3m + 
                media_count.3m +
                i(sentiment, ref = "neutral") +
                i(weekend, ref = "weekday") + 
                length + 
                question.mark + 
                conversations |
                agencyname^year,
              family = "logit",
              data = reg)

summary(f1.5)

# Model 1.6: Continuous real  time as random slope
f1.6 <- feglm(response.num ~ 
                valence.3m + 
                media_count.3m +
                i(sentiment, ref = "neutral") +
                i(weekend, ref = "weekday") + 
                length + 
                question.mark + 
                conversations |
                agencyname[time],
              family = "logit",
              data = reg)

summary(f1.6)

# Select model with best fit

# Model 1.7: conversation as cluster
f1.7 <- update(f1.4, cluster = ~ conversation_id,
              family = "logit",
              data = reg)

summary(f1.7)

# Model 1.8: Time & agency as cluster
f1.8 <- update(f1.5, cluster = ~ year^agencyname,
               family = "logit",
               data = reg)

summary(f1.8)

# Model comparison
etable(f1.4, f1.7, f1.8)

# Look at fixed effects
fp1.4 <- fixef(f1.4)
plot(fp1.4)

fp1.7 <- fixef(f1.7)
plot(fp1.7)

fp1.8 <- fixef(f1.8)
plot(fp1.8)

# Quick export
modelsummary(f1.4)

# ======================================================
#           Random effects
# ======================================================

# To do: check polynomial for time

# Model 3.0: Empty model
l3.0 <- glm(response.num ~ 1,
              na.action = na.omit,
              data = reg,
              family = binomial(link = "logit"))

summary(l3.0)

# Model 3.3: Adding level-1 variables
l3.1 <- update(l3.0, . ~ . +
                 sentiment +
                 weekend + 
                 length + 
                 question.mark)
summary(l3.1)
anova(l3.1)

# Model 3.2: Adding level-2 controls
l3.2 <- update(l3.1, . ~ . +
                 time.1000 +
                 valence.3m + 
                 media_count.3m +
                 conversations)
summary(l3.2)
anova(l3.2)

# Model 3.3: Adding conversation as random effect
l3.3 <- glmer(response.num ~ 
                sentiment + 
                weekend + 
                question.mark + 
                valence.3m.s + 
                media_count.3m.s + 
                conversations.s +
                (1 | agencyname),
              na.action = na.omit,
              data = reg %>% filter(agencyname != "EIOPA"),
              family = binomial(link = "logit"))

summary(l3.3)
performance::icc(l3.3)
anova(l3.2, l3.3)

# Model 3.4: Adding conversation as random effect
l3.4 <- update(l3.3, . ~ . -
                 (1 | agencyname) +
                 (1 |conversation_id))

summary(l3.4)
performance::icc(l3.4)
anova(l3.2, l3.4)

# Model 3.5: Adding agency as third level
l3.5 <- update(l3.3, . ~ . + (1 | agencyname:conversation_id))

summary(l3.5)
performance::icc(l3.5, by_group = TRUE) # Variance by group

anova(l3.3, l3.5)

# Model 3.6: Agency as FE instead of RE
l3.6 <- update(l3.4, . ~ . + 
                 agencyname)

summary(l3.6)
performance::icc(l3.6) 

anova(l3.4, l3.6)

summary(l3.6)


# ======================================================
#           Panel agency-by-week
# ======================================================

m1.0 <- pglm(information ~ 
               lag(information.cgm) +
               lag(information.cgm,  12),
             data = pdata,
             model = "within",
             family = "negbin")

summary(m1.0)

m1.1 <- update(m1.0, . ~ . +
                 audience_count.lcgm +
                 audience_threat.cgm +
                 lag(audience_count.lcgm) +
                 lag(audience_threat.cgm))

summary(m1.1)

m1.2 <- update(m1.0, . ~ . +
                 media_count.lcgm +
                 lag(media_count.lcgm) +
                 media_count.cgm +
                 lag(media_count.cgm) +
                 media_count.3.cgm +
                 media_count.12.cgm +
                 media_threat.cgm +
                 lag(media_threat.cgm))
summary(m1.2)

m1.3 <- update(m1.0, . ~ . +
                 valence.3 + 
                 valence.12) # No effect...

summary(m1.3)

# m1.4 <- update(m1.0, . ~ . +
#                 hearing +
#                 lag(hearing)) # Needs fixing, see above

# summary(m1.4)

m1.5 <- update(m1.0, . ~ . +
                 lag(audience_count.lcgm) +
                 lag(audience_threat.cgm) +
                 valence.3.cgm +
                 valence.3.cgm:lag(audience_count.lcgm),
               valence.3.cgm:lag(audience_threat.cgm))

summary(m1.5)

m1.6 <- update(m1.0, . ~ . +
                 media_count.lcgm +
                 valence.3.cgm +
                 valence.3.cgm:media_count.lcgm)
summary(m1.6)

m1.7 <- update(m1.0, . ~ . +
                 media_count.lcgm +
                 valence.3.cgm +
                 lag(audience_count.lcgm) +
                 valence.3.cgm:media_count.lcgm +
                 valence.3.cgm:lag(audience_count.lcgm))
summary(m1.7)

m1.8 <- update(m1.7, . ~ . +
                 lag(audience_threat.cgm))

summary(m1.8)

f1.8a <- information ~
  lag(information) +
  lag(information, 12) + 
  valence.3 +
  media_count.l +
  lag(audience_count.l) +
  valence.3.cwc:media_count.lcwc +
  lag(audience_count.lcwc):valence.3.cwc +
  lag(audience_threat)

# Note:
## lag of 1: p = 0.075
## media_count.3: p = 0.13
## media_count.12: p = 0.022
# Note: valence.3:lag(audience_threat) = n.s.

# Getting marginal effects

m1.8a <- pglm(f1.8a,
              data = pdata,
              family = 'negbin', 
              model = "within")

summary(m1.8a)

# See https://github.com/benjaminguinaudeau/margins.pglm
# TO DO: Check warnings
# NOTE: SE/z/p/CIs seems incorrect. Contact author?

summary(margins::margins(model = m1.8a, formula = f1.8a))

## Audience
m.audience <- margins::margins(model = m1.8a,
                               at = list("valence.3.cwc" = c(seq(from = -1.3, to = 0.7, by = 0.1))),
                               variables = "audience_count.lcwc",
                               formula = f1.8a)

ggplot(aes(x = valence.3.cwc,
           y = AME,
           ymin = lower,
           ymax = upper),
       data = summary(m.audience)) +
  geom_point() +
  geom_linerange() +
  labs(y = "Average Marginal Effect of \n audience_count (log)",
       title = "Information") +
  theme_bw()


##  Media
m.media <- margins::margins(model = m1.8a,
                            at = list("valence.3.cwc" = c(seq(from = -1.3, to = 0.7, by = 0.1))),
                            variables = "media_count.lcwc",
                            formula = f1.8a)
ggplot(aes(x = valence.3.cwc,
           y = AME,
           ymin = lower,
           ymax = upper),
       data = summary(m.media)) +
  geom_point() +
  geom_linerange() +
  labs(y = "Average Marginal Effect of \n media_count",
       title = "Information") +
  theme_bw()

# Alternative: panelr

rdata <- tdata %>%
  mutate(agencyname = as.ordered(as.factor(agencyname)),
         month = as.ordered(as.factor(month)))

rdata <- panel_data(rdata, id = agencyname, wave = month)

pdim(rdata)

m1.8b <- wbm(information ~
               lag(information.cgm) +
               lag(information.cgm, 12) + 
               valence.3.cgm +
               media_count.lcgm +
               lag(audience_count.lcgm) +
               valence.3.cgm:media_count.lcgm +
               lag(audience_count.lcgm):valence.3.cgm +
               lag(audience_threat.cgm),
             data = rdata,
             interaction.style = "double-demean",
             model = "within",
             family = "gaussian")

summary(m1.8b)

# Note: 'raw' & 'double-demean' produce nearly identical results -> both interactions are n.s.
# Only 'demean' produces (highly) significant results.
# Setting interaction.style to 'demean' produces results very close to those from pglm

m1.8c <- wbm(information.log ~
               lag(information.log) +
               lag(information.log, 12) + 
               valence.3 +
               media_count +
               lag(audience_count.log) +
               valence.3:media_count +
               lag(audience_count.log):valence.3 +
               lag(audience_threat),
             data = rdata,
             interaction.style = "double-demean",
             model = "within",
             family = "gaussian")

summary(m1.8c)

# GLMMTMB
m1.8d <- glmmTMB(information ~
                   valence.3.cwc*media_count.lcwc +
                   audience_count.lcwc*valence.3.cwc +
                   (1 | agencyname) +
                   ar1(month + 0 | agencyname),
                 data = tdata,
                 family = nbinom2)
help("convergence")
summary(m1.8d)

# Alternative: GLMMadaptive
m1.8e <- mixed_model(fixed = information ~
                       valence.3.cwc*media_count.lcwc +
                       audience_count.lcwc*valence.3.cwc,
                     random = ~ random.num | agencyname,
                     data = tdata,
                     family = negative.binomial())

summary(m1.8e)
m1.8f <- update(m1.8e, nAGQ = 21)

fixef(m1.8f)
ranef(m1.8f)

# ======================================================
#           Responsiveness
# ======================================================

# Distribution
ggplot(data, aes(x = response)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
                 binwidth = 1,
                 colour = "black",
                 fill = "white") +
  theme_clean() + 
  geom_density(alpha = .2, fill="#FF6655")

# Offset does not work in pglm
# See: https://stackoverflow.com/questions/55966561/pglm-fixed-effect-poisson-model-with-offset

# Solution: different data format:
# 1) Tweet - agency, with dummy DV varying between 'response' and 'no response'
# 2) Panelr can be used to specify offset