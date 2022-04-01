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
library(texreg)
library(interactions)
library(effects)

# To do:
# Check other operationalisations of twitter sentiment (e.g., moar, etc.)
# Set time zone

load("./data/response_panel2.Rda")
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
  arrange(agencyname, week.number) %>%
  group_by(agencyname) %>%
  dplyr::mutate(
         
         # Cubic root
         twitter.index1.cr = cubic.root(twitter.index1),
        
         # Media count log
         media.source1.log = log10(media.source1 + 1),
         interaction1.conv = media.source1*twitter.index1) %>%
  ungroup() %>%
  drop_na(media.source1)

data.wb.temp <-  cbind(panel.new,
                  datawizard::demean(panel.new, 
                      select =
                      c("twitter.index1", 
                        "media.source1"),
                        group = "agencyname")) %>%
  mutate(interaction1.double = media.source1_within*twitter.index1_within)

data.wb <- cbind(data.wb.temp,
                 datawizard::demean(data.wb.temp,
                                    select = c("interaction1.double"),
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
                  ziformula = ~ 1,
                  data = data.wb,
                  family = nbinom2)

summary(nb2.7g)
compare_performance(nb2.6g, nb2.7g, metrics = c("AIC", "BIC", "ICC"))
lrtest(nb2.g, nb2.7g)

# Now with transformed Twitter index (glmmTMB)
nb2.8g <- glmmTMB(response.week ~
                    twitter.index1.cr_within +
                    media.source1_within +
                    interaction1.cr_within +
                    twitter.index1.cr_between +
                    media.source1_between +
                    poly(time.on.Twitter, 2) +
                    offset(log(offset.week + 1)) +
                    (1 | agencyname) +
                    ar1(0 + week.factor | agencyname),
                  ziformula = ~ 1,
                  data = data.wb,
                  family = nbinom2)

summary(nb2.8g)
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

## MODEL EVALUATION
testDispersion(nb2.7g)
testDispersion(nb2.8g)

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



# balance.correction	& detrend
# Adjust within-subject effects for trends in the predictors? Default is FALSE, but some research suggests this is a better idea (see Curran and Bauer (2011) reference).
# Correct between-subject effects for unbalanced panels following the procedure in Curran and Bauer (2011)? Default is FALSE