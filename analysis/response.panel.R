
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

probe_interaction() #  https://rdrr.io/cran/interactions/man/probe_interaction.html

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