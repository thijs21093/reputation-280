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

load("./data/response_panel.Rda")

# To do: remove same week in data cleaning script
# Check NAs in media.source and no.of.sources
 ## EMCDDA and EUROFOUND
# Fix lag

# ======================================================
#           Panel agency-by-week
# ======================================================
panel.new <- response.panel %>%
  drop_na() %>% 
  ungroup() %>%
  filter(agencyname != "EIOPA") %>% # Only zeros
  dplyr::mutate(
        # Scale information
    information.s = information/100,

    # Scale Twitter index
    twitter.index.s = twitter.index/100,
    
    # Media valence
    media.valence = ifelse(!(positive + neutral + negative), 0, (positive - negative) / (positive + neutral + negative))) # Move to other script
    
# Descriptives
# ======================================================
var.table.t <- panel.new %>% 
  dplyr::select(
    response.week, twitter.index.s, media.source, media.valence, 
      short.ratio,
      qm.comment.ratio,
      attachment.ratio,
      information.s,
      weekend.ratio,
      mention.ratio,
      qm.agency.ratio,
    offset.week) %>%
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

# Creating a panel dataset
panel <- panel_data(panel.new, id = agencyname, wave = week) %>%
  mutate(media.source.lag	= media.source)

pdim(panel)

# ======================================================
#           Panel
# ======================================================

# Null model
p0 <- wbm(response.week ~ 1,
          offset = log(offset.week + 1),
          data = panel,
          model = "within",
          family = "poisson")

summary(p0)

# Add vars of interest
p1 <-  wbm(response.week ~
             plm::lag(twitter.index.s, 12) + 
             plm::lag(media.source, 12) +
             plm::lag(media.valence, 12),
           offset = log(offset.week + 1),
           data = panel,
           model = "within",
           family = "poisson")
  
summary(p1)

anova(p0, p1)

# Add interaction
p2 <- wbm(response.week ~
          plm::lag(twitter.index.s, 12) * plm::lag(media.source, 12) * plm::lag(media.valence, 12),
          offset = log(offset.week + 1),
          data = panel,
          model = "within",
          family = "poisson",
          interaction.style = "double-demean")

summary(p2)
anova(p1, p2)

# probe_interaction(p2)
#  https://rdrr.io/cran/interactions/man/probe_interaction.html


# Add controls
p3 <- wbm(response.week ~
          plm::lag(twitter.index.s, 12) * plm::lag(media.source, 12) * plm::lag(media.valence, 12) +
          short.ratio +
          qm.comment.ratio +
          attachment.ratio +
          information.s +
          weekend.ratio	+
          mention.ratio	+
          qm.agency.ratio,
          offset = log(offset.week + 1),
          data = panel,
          model = "within",
          family = "poisson",
          interaction.style = "double-demean")

summary(p3)
anova(p2, p3)

# Add time
p4 <- wbm(response.week ~
            plm::lag(twitter.index.s, 12) * plm::lag(media.source, 12) * plm::lag(media.valence, 12) +
            short.ratio +
            qm.comment.ratio +
            attachment.ratio +
            information.s,
          offset = log(offset.week + 1),
          data = panel,
          model = "within",
          family = "poisson",
          use.wave = TRUE,
          interaction.style = "double-demean")

summary(p4)
anova(p3, p4)


# Detrend
# detrend
# balance.correction	
# Correct between-subject effects for unbalanced panels following the procedure in Curran and Bauer (2011)? Default is FALSE
# Negbin?


# See https://github.com/benjaminguinaudeau/margins.pglm
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