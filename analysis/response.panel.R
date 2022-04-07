# Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(sjmisc)
library(kableExtra)
library(sjPlot)
library(panelr)
library(modelsummary)
library(plm)
library(zoo)
library(datawizard)
library(glmmTMB)
library(performance)
library(lmtest)
library(DHARMa)
<<<<<<< HEAD

# Set time zone
Sys.setenv(TZ = 'GMT')

load("./data/response_panel.Rda")

# Fix lag

##  Main vars:
# DVs
# response.week = sum(response)

# Offset
# offset.week = n()

# IVs
# media.source = media salience
# twitter.valence = twitter.praise - twitter.criticism
# time.on.Twitter = difftime(strptime(week, format = "%Y-%m-%d"), strptime(join.day, format = "%Y-%m-%d"), units = c("weeks"))

=======
library(texreg)
library(interactions)
library(effects)

# To do:
# Check other operationalisations of twitter sentiment (e.g., moar, etc.)
# Set time zone

load("./data/response_panel2.Rda")
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
# ======================================================
#           Panel agency-by-week
# ======================================================
cubic.root <- function(x) {
  sign(x) * abs(x)^(1/3)
}

panel.new <- response.panel %>%
  ungroup() %>%
  mutate(week.factor = as.factor(week),
         week.number = as.numeric(week.factor)) %>%
  filter(agencyname != "EIOPA") %>% # Only zeros
<<<<<<< HEAD
  dplyr::mutate(
        # Scale information
    information.s = information/100,

    # Scale Twitter index
    twitter.index = twitter.valence)  %>% # Valence -> index! Change to index when this is corrected in data cleaning script
  arrange(agencyname, week.number) %>%
  group_by(agencyname) %>%
  dplyr::mutate(
         # Responsiveness
          response.week1 = rollapplyr(response.week, list(seq(-1, -1)), sum, fill = NA, align = "right"),

          
          response.rate = if_else(offset.week  != 0 , response.week/offset.week, 0),
          response.rate.log = log(response.rate + 1),
          
         # Twitter index
         twitter.index1 = rollapplyr(twitter.index, list(seq(-1, -1)), sum, fill = NA, align = "right"),
         twitter.index2 = rollapplyr(twitter.index, list(seq(-2, -1)), sum, fill = NA, align = "right"),
         twitter.index3 = rollapplyr(twitter.index, list(seq(-3, -1)), sum, fill = NA, align = "right"),
         twitter.index4 = rollapplyr(twitter.index, list(seq(-4, -1)), sum, fill = NA, align = "right"),
         twitter.index5 = rollapplyr(twitter.index, list(seq(-5, -1)), sum, fill = NA, align = "right"),
         twitter.index6 = rollapplyr(twitter.index, list(seq(-6, -1)), sum, fill = NA, align = "right"),
         twitter.index7 = rollapplyr(twitter.index, list(seq(-7, -1)), sum, fill = NA, align = "right"),
         twitter.index8 = rollapplyr(twitter.index, list(seq(-8, -1)), sum, fill = NA, align = "right"),
         twitter.index9 = rollapplyr(twitter.index, list(seq(-9, -1)), sum, fill = NA, align = "right"),
         twitter.index10 = rollapplyr(twitter.index, list(seq(-10, -1)), sum, fill = NA, align = "right"),
         twitter.index11 = rollapplyr(twitter.index, list(seq(-11, -1)), sum, fill = NA, align = "right"),
         twitter.index12 = rollapplyr(twitter.index, list(seq(-12, -1)), sum, fill = NA, align = "right"),
         twitter.index13 = rollapplyr(twitter.index, list(seq(-13, -1)), sum, fill = NA, align = "right"),
         twitter.index26 = rollapplyr(twitter.index, list(seq(-25, -1)), sum, fill = NA, align = "right"),
         twitter.index52 = rollapplyr(twitter.index, list(seq(-52, -1)), sum, fill = NA, align = "right"),
         
         # Cubic root
         twitter.index1.cr = cubic.root(twitter.index1),
         twitter.index2.cr = cubic.root(twitter.index2),
         twitter.index3.cr = cubic.root(twitter.index3),
         twitter.index4.cr = cubic.root(twitter.index4),
         twitter.index5.cr = cubic.root(twitter.index5),
         twitter.index6.cr = cubic.root(twitter.index6),
         twitter.index7.cr = cubic.root(twitter.index7),
         twitter.index8.cr = cubic.root(twitter.index8),
         twitter.index9.cr = cubic.root(twitter.index9),
         twitter.index10.cr = cubic.root(twitter.index10),
         twitter.index11.cr = cubic.root(twitter.index11),
         twitter.index12.cr = cubic.root(twitter.index12),
         twitter.index13.cr = cubic.root(twitter.index13),
         twitter.index26.cr = cubic.root(twitter.index26),
         twitter.index52.cr = cubic.root(twitter.index52),

         # Media count
         media.source1 = rollapplyr(media.source, list(seq(-1, -1)), sum, fill = NA, align = "right"),
         media.source2 = rollapplyr(media.source, list(seq(-2, -1)), sum, fill = NA, align = "right"),
         media.source3 = rollapplyr(media.source, list(seq(-3, -1)), sum, fill = NA, align = "right"),
         media.source4 = rollapplyr(media.source, list(seq(-4, -1)), sum, fill = NA, align = "right"),
         media.source5 = rollapplyr(media.source, list(seq(-5, -1)), sum, fill = NA, align = "right"),
         media.source6 = rollapplyr(media.source, list(seq(-6, -1)), sum, fill = NA, align = "right"),
         media.source7 = rollapplyr(media.source, list(seq(-7, -1)), sum, fill = NA, align = "right"),
         media.source8 = rollapplyr(media.source, list(seq(-8, -1)), sum, fill = NA, align = "right"),
         media.source9 = rollapplyr(media.source, list(seq(-9, -1)), sum, fill = NA, align = "right"),
         media.source10 = rollapplyr(media.source, list(seq(-10, -1)), sum, fill = NA, align = "right"),
         media.source11 = rollapplyr(media.source, list(seq(-11, -1)), sum, fill = NA, align = "right"),
         media.source12 = rollapplyr(media.source, list(seq(-12, -1)), sum, fill = NA, align = "right"),
         media.source13 = rollapplyr(media.source, list(seq(-13, -1)), sum, fill = NA, align = "right"),
         media.source26 = rollapplyr(media.source, list(seq(-25, -1)), sum, fill = NA, align = "right"),
         media.source52 = rollapplyr(media.source, list(seq(-52, -1)), sum, fill = NA, align = "right"),
         
         # Media count log
         media.source1.log = log2(media.source1 + 1),
         media.source2.log = log2(media.source2 + 1),
         media.source3.log = log2(media.source3 + 1),
         media.source4.log = log2(media.source4 + 1),
         media.source5.log = log2(media.source5 + 1),
         media.source6.log = log2(media.source6 + 1),
         media.source7.log = log2(media.source7 + 1),
         media.source8.log = log2(media.source8 + 1),
         media.source9.log = log2(media.source9 + 1),
         media.source10.log = log2(media.source10 + 1),
         media.source11.log = log2(media.source11 + 1),
         media.source12.log = log2(media.source12 + 1),
         media.source13.log = log2(media.source13 + 1),
         media.source26.log = log2(media.source26 + 1),
         media.source52.log = log2(media.source52 + 1),
         
=======
  arrange(agencyname, week.number) %>%
  group_by(agencyname) %>%
  dplyr::mutate(
         
         # Cubic root
         twitter.index1.cr = cubic.root(twitter.index1),
        
         # Media count log
         media.source1.log = log10(media.source1 + 1),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
         interaction1.conv = media.source1*twitter.index1) %>%
  ungroup() %>%
  drop_na(media.source1)

data.wb.temp <-  cbind(panel.new,
                  datawizard::demean(panel.new, 
                      select =
                      c("twitter.index1", 
<<<<<<< HEAD
                        "twitter.index1.cr", 
                        "media.source1", 
                        "media.source1.log",
                        "media.source4"),
                        group = "agencyname")) %>%
  mutate(interaction1.double = media.source1_within*twitter.index1_within,
         interaction1_4.double = media.source4_within*twitter.index1_within,
         interaction1.trans_double = media.source1.log_within*twitter.index1.cr_within,
         interaction1.log_double = media.source1.log_within*twitter.index1_within)
data.wb <- cbind(data.wb.temp,
                 datawizard::demean(data.wb.temp,
                                    select = c("interaction1.double", "interaction1.trans_double", "interaction1.log_double", "interaction1_4.double"),
=======
                        "media.source1"),
                        group = "agencyname")) %>%
  mutate(interaction1.double = media.source1_within*twitter.index1_within)

data.wb <- cbind(data.wb.temp,
                 datawizard::demean(data.wb.temp,
                                    select = c("interaction1.double"),
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
                                    group = "agencyname")) 


# Descriptives
# ======================================================
# Within
var.within <- data.wb %>%
  dplyr::select(contains("within")) %>%
  descr() %>% 
  data.frame() %>% 
  dplyr::select(-type,
                -var,
                -NA.prc,
                -se,
                -trimmed,
                -iqr) %>%
  mutate_at(vars(c("mean","sd","md")),funs(round(.,3)))

var.within %>%
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

# Between
var.between <- data.wb %>%
  dplyr::select(contains("between")) %>%
  descr() %>% 
  data.frame() %>% 
  dplyr::select(-type,
                -var,
                -NA.prc,
                -se,
                -trimmed,
                -iqr) %>%
  mutate_at(vars(c("mean","sd","md")),funs(round(.,3)))

var.between %>%
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

# Creating a panel dataset
panel <- panel_data(panel.new, id = agencyname, wave = time.on.Twitter) 
pdim(panel)

<<<<<<< HEAD
=======
# ????
data.alt <- make_wb_data(response.week ~ media.source1*twitter.index1,
                 offset = log(offset.week + 1),
                 data = panel,
                 wave = TRUE,
                 detrend = TRUE,
                 balance.correction = TRUE,
                 model = "w-b",
                 interaction.style = "double-demean",
                 family = "poisson")

>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
# ======================================================
#           Panel
# ======================================================
# Null model: poisson (glmmTMB)
p0g <- glmmTMB(response.week ~ 1 +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname),
               data = data.wb,
               family = poisson)
summary(p0g)

# Checking the effect of detrending: poisson (panelr)
p1p.no.trend <- wbm(response.week ~ media.source1*twitter.index1,
                 offset = log(offset.week + 1),
                 data = panel,
                 model = "within",
                 detrend = FALSE,
                 interaction.style = "double-demean",
                 family = "poisson")

summary(p1p.no.trend)

# Checking the effect of detrending: poisson (panelr)
<<<<<<< HEAD
=======
p2p.int.plot <- glmmTMB(response.week ~ 1 +
                media.source
                 offset(log(offset.week + 1)) +
                 (1 | agencyname),
               data = data.wb,
               family = poisson)
summary(p0g)


summary(p2p.int.plot)

interact_plot(p2p.int.plot,
                   modx = media.source1,
                   pred = twitter.index1)

plot_model(p2p.int.plot, "int") 
plot_model(p2p.int.plot, type = "pred", terms = c("media.source1", "media.source1"))

# Checking the effect of detrending: poisson (panelr)
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
p1p.trend <- wbm(response.week ~ media.source1*twitter.index1,
           offset = log(offset.week + 1),
           data = panel,
           model = "within",
           detrend = TRUE,
           interaction.style = "double-demean",
           family = "poisson")

summary(p1p.trend)
compare_performance(p1p.trend, p1p.no.trend, metrics = c("AIC", "BIC", "ICC"))

# VERY SMALL DIFFERENCE

# Adding vars of interest: poisson (panelr)
p1p <- wbm(response.week ~ media.source1*twitter.index1,
           offset = log(offset.week + 1),
           data = panel,
           model = "within",
           use.wave = TRUE, # Raw value
           wave.factor = FALSE,
           interaction.style = "double-demean",
           family = "poisson")

summary(p1p)

# Adding vars of interest + interaction: poisson (glmmTMB)
p1g <- glmmTMB(response.week ~ 1 + 
              media.source1_within +
              twitter.index1_within +
              interaction1.double_within +
              offset(log(offset.week + 1)) +
              (1 | agencyname),
              data = data.wb,
              family = poisson)

summary(p1g)
compare_performance(p1p, p1g, metrics = c("AIC", "BIC", "ICC"))

# Adding time: poisson (glmmTMB)
p2g <- glmmTMB(response.week ~ 1 + 
                 media.source1_within +
                 twitter.index1_within +
                 interaction1.double_within +
                 poly(time.on.Twitter, 2) +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname),
               data = data.wb,
               family = poisson)

summary(p2g)
compare_performance(p1g, p2g, metrics = c("AIC", "BIC", "ICC"))

# Adding ar1: poisson (glmmTMB)
p3g <- glmmTMB(response.week ~ 1 + 
                 media.source1_within +
                 twitter.index1_within +
                 interaction1.double_within +
                 poly(time.on.Twitter, 2) +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname) +
                 ar1(0 + week.factor | agencyname),
               data = data.wb,
               family = poisson)

summary(p3g)
compare_performance(p2g, p3g, metrics = c("AIC", "BIC", "ICC"))

# Adding between effects: poisson (glmmTMB)
p4g <- glmmTMB(response.week ~
                 twitter.index1_within +
                 media.source1_within +
                 interaction1.double_within +
                 twitter.index1_between +
                 media.source1_between +
                 poly(time.on.Twitter, 2) +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname) +
                 ar1(0 + week.factor | agencyname),
               data = data.wb,
               family = poisson)

summary(p4g)
compare_performance(p3g, p4g, metrics = c("AIC", "BIC", "ICC"))
lrtest(p3g, p4g)

# Zero-inflated Poisson (glmmTMB)
p5g <- glmmTMB(response.week ~
                 twitter.index1_within +
                 media.source1_within +
                 interaction1.double_within +
                 twitter.index1_between +
                 media.source1_between +
                 poly(time.on.Twitter, 2) +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname) +
                 ar1(0 + week.factor | agencyname),
               ziformula = ~ 1,
               data = data.wb,
               family = poisson)

summary(p5g)
compare_performance(p4g, p5g, metrics = c("AIC", "BIC", "ICC"))
lrtest(p4g, p5g)

# Zero-inflated Negative binomial 1 (glmmTMB)
nb1.5g <- glmmTMB(response.week ~
                 twitter.index1_within +
                 media.source1_within +
                 interaction1.double_within +
                 twitter.index1_between +
                 media.source1_between +
                 poly(time.on.Twitter, 2) +
                 offset(log(offset.week + 1)) +
                 (1 | agencyname) +
                 ar1(0 + week.factor | agencyname),
               ziformula = ~ 1,
               data = data.wb,
               family = nbinom1)

summary(nb1.5g)
compare_performance(p5g, nb1.5g, metrics = c("AIC", "BIC", "ICC"))
lrtest(p5g, nb1.5g)


# Zero-inflated Negative binomial 2 (glmmTMB)
nb2.5g <- glmmTMB(response.week ~
                    twitter.index1_within +
                    media.source1_within +
                    interaction1.double_within +
                    twitter.index1_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
                  ziformula = ~ 1,
                  data = data.wb,
                  family = nbinom2)

summary(nb2.5g)
compare_performance(nb1.5g, nb2.5g, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb1.5g, nb2.5g)

# Zero-inflated Negative binomial 2 (glmmTMB)
nb2.6g <- glmmTMB(response.week ~
                    twitter.index1_within +
                    media.source1_within +
                    interaction1.double_within +
                    twitter.index1_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (twitter.index1_within | agencyname) +
                    ar1(0 + week.factor | agencyname),
                  ziformula = ~ 1,
                  data = data.wb,
                  family = nbinom2)

summary(nb2.6g)
compare_performance(nb2.5g, nb2.6g, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb2.5g, nb2.6g)

# Zero-inflated Negative binomial 2 with extended zi model (glmmTMB)

## NO SIGNIFICANT IMPROVEMENT ##
nb2.6g <- glmmTMB(response.week ~
                    twitter.index1_within +
                    media.source1_within +
                    interaction1.double_within +
                    twitter.index1_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
                  ziformula = ~ (1 | agencyname),
                  data = data.wb,
                  family = nbinom2)


summary(nb2.6g)
compare_performance(nb2.5g, nb2.6g, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb2.5g, nb2.6g)

# Zero-inflated Negative binomial 2 with random effects in zi model (glmmTMB)
nb2.7g <- glmmTMB(response.week ~
                    twitter.index1_within +
                    media.source1_within +
                    interaction1.double_within +
                    twitter.index1_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
<<<<<<< HEAD
                  ziformula = ~ (1 | agencyname),
=======
                  ziformula = ~ 1,
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
                  data = data.wb,
                  family = nbinom2)

summary(nb2.7g)
compare_performance(nb2.6g, nb2.7g, metrics = c("AIC", "BIC", "ICC"))
<<<<<<< HEAD
lrtest(nb2.6g, nb2.7g)

# Transformed media variable  (glmmTMB)
nb2.8g <- glmmTMB(response.week ~
                    twitter.index1_within +
                    media.source4_within +
                    interaction1_4.double_within +
                    twitter.index1_between +
=======
lrtest(nb2.g, nb2.7g)

# Now with transformed Twitter index (glmmTMB)
nb2.8g <- glmmTMB(response.week ~
                    twitter.index1.cr_within +
                    media.source1_within +
                    interaction1.cr_within +
                    twitter.index1.cr_between +
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
<<<<<<< HEAD
                  ziformula = ~ (1 | agencyname),
=======
                  ziformula = ~ 1,
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
                  data = data.wb,
                  family = nbinom2)

summary(nb2.8g)
<<<<<<< HEAD
=======
compare_performance(nb2.6g, nb2.7g, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb2.6g, nb2.7g)


# Model for plotting interaction (simple)
nb2.7g.inta <- glmmTMB(response.week ~
                         media.source1_within*twitter.index1_within +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname),
                  data = data.wb,
                  family = nbinom2)

summary(nb2.7ga)
compare_performance(nb2.7g, nb2.7ga, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb2.7g, nb2.7ga)

# Model for plotting interaction (glmmTMB)
nb2.7g.intb <- glmmTMB(response.week ~
                    twitter.index1_within*media.source1_within +
                    twitter.index1_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
                  ziformula = ~ 1,
                  data = data.wb,
                  family = nbinom2)

summary(nb2.7g.intb)
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

## MODEL EVALUATION
testDispersion(nb2.7g)
testDispersion(nb2.8g)
<<<<<<< HEAD
testDispersion(nb2.test)

=======

# Extracting information

## Poisson
extract.nb2.7g <- texreg::extract(nb2.7g,  include.variance = TRUE)

# Calculating IRRs
coefs <- extract.nb2.7g@coef
exp.coefs <- exp(extract.nb2.7g@coef)

# Combined: Poisson
htmlreg(list(extract.nb2.7g),
        custom.model.names = c("Model"),
        caption = "TABLE: Generalized Linear Mixed Models (GLMM) with AR1 covariance structure.",
        caption.above = TRUE,
        ci.force = TRUE,
        ci.test = 0,
        ci.force.level = 0.95,
        bold = TRUE,
        float.pos = "h",
        booktabs = TRUE,
        custom.note = "Note:.",
        digits = 3,
        include.pvalues = TRUE, 
        single.row = TRUE,
        file = 'zi-nb2.doc')

# ======================================================
#           EFFECTS
# ======================================================


## Note: Check if offset is incorporated correctly in plots
plot_model(nb2.7g.intb,
                               type = "int",
                               show.legend = TRUE,
                               line.size = 1,
                               mdrt.values = "meansd",
                               ci.lvl = NA) +
  theme_few()  +
  theme(legend.position = c(.2, .8),
        legend.background = element_rect(colour = "black", fill = "white"))

ae <- allEffects(nb2.7g, se = FALSE)
plot(ae)

# Twitter
plot_model(nb2.7g,  type = "eff", terms = "twitter.index1_within", ci.lvl = NA) 
plot_model(nb2.7g,  type = "eff", terms = "twitter.index1_between", ci.lvl = NA) 

# Media
plot_model(nb2.7g,  type = "eff", terms = "media.source1_within", ci.lvl = NA) 
plot_model(nb2.7g,  type = "eff", terms = "media.source1_between", ci.lvl = NA) 

# Time
plot_model(nb2.7g,  type = "eff", terms = "time.on.Twitter", ci.lvl = NA) 
interact_plot(nb2.7g,
              modx = twitter.index1_within,
              pred = media.source1_within,
              data = data.wb)
>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e

# Simulate residuals
simulation.nb2.7g <- simulateResiduals(fittedModel = nb2.7g, plot = F)
simulation.nb2.8g <- simulateResiduals(fittedModel = nb2.7g, plot = F)
simulation.nb2.test <- simulateResiduals(fittedModel = nb2.test, plot = F)

plot(simulation.nb2.7g)
plot(simulation.nb2.8g)

testUniformity(simulation.nb2.7g) # tests if the overall distribution conforms to expectations
testUniformity(simulation.nb2.8g) # tests if the overall distribution conforms to expectations
testOutliers(simulation.nb2.7g) # tests if there are more simulation outliers than expected
testDispersion(simulation.nb2.7g) # tests if the simulated dispersion is equal to the observed dispersion
testQuantiles(simulation.nb2.7g) # fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
plotResiduals(simulation.nb2.7g, data.wb$media.source1_within)
plotResiduals(simulation.nb2.7g, data.wb$media.source1_between)
plotResiduals(simulation.nb2.7g, data.wb$twitter.index1_within)
plotResiduals(simulation.nb2.7g, data.wb$twitter.index1_between)

testZeroinflation() # tests if there are more zeros in the data than expected from the simulations

## Plotting the interaction

<<<<<<< HEAD
=======


>>>>>>> c8cc2f359ba27695940a8b499455dcd5f78c029e
# balance.correction	& detrend
# Adjust within-subject effects for trends in the predictors? Default is FALSE, but some research suggests this is a better idea (see Curran and Bauer (2011) reference).
# Correct between-subject effects for unbalanced panels following the procedure in Curran and Bauer (2011)? Default is FALSE