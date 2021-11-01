#libraries
library(tidyverse)
library(anomalize)
library(dplyr)
library(ggplot2)

# Load data
load("./Data/audiencecounts")
for (i in seq(audiencecounts))
  assign(paste0("df", i), audiencecounts[[i]]) # Create seperate dataframes

dfs <- sapply(.GlobalEnv, is.data.frame) # Find dataframes in enviroment
df <- do.call(rbind, mget(names(dfs)[dfs])) # Bind dataframes

df <- df %>%
  mutate(start = as.POSIXct(start),
         end = as.POSIXct(end)) %>% # Adjust time variables
  group_by(agencyname) %>% # Group by agency
  select(-agencyhandle) # Remove handle

# Plot time 
df %>% ggplot(aes(x = start, y = tweet_count)) +
  facet_wrap(~agencyname,
             ncol = 4,
             scales = "free") +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 0.5) +
  geom_point(size = 2) +
  theme_few() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 10)) +
  xlab("Time") + ylab("Count")

# Set count to NA for which an agency has no Twitter


# Convert df to a tibble
df <- as_tibble(df)
class(df)

df_anomalized <- df %>%
  time_decompose(overall, merge = TRUE) %>%
  anomalize(remainder) %>%
  anomalize::time_recompose()

# Glimpse
df_anomalized %>% glimpse()

# Plot
df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

# Adjusting Trend and Seasonality
p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
p1

# Adjusting local parameters
p2 <- df %>%
  time_decompose(overall,
                 frequency = "auto",
                 trend     = "2 weeks") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Local)")
p2

# Adjusting the global parameter
time_scale_template() %>%
  mutate(trend = ifelse(time_scale == "day", "2 weeks", trend)) %>%
  set_time_scale_template()
get_time_scale_template() # View template


p3 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Global)")
p3

time_scale_template() %>%
  set_time_scale_template() # Reset template
get_time_scale_template() # Verify the change

# Extracting the anomalous data Points
df %>% 
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

## Adjusting alpha and max anoms

# Alpha
p4 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p4

p5 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder, alpha = 0.025, max_anoms = 0.2) %>% # alpha = 0.025
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p5


# Max anomalies
p6 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.2) %>% # Note: percentages
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("20% Anomalies")
p6
#> frequency = 7 days
#> trend = 91 days
p7 <- df %>%
  time_decompose(overall) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("5% Anomalies")
#> frequency = 7 days
#> trend = 91 days
p7

##  ‘timetk’ package
# Interactive anomaly visualization
df %>% timetk::plot_anomaly_diagnostics(month,overall, .facet_ncol = 2)

# Extraction
df %>% timetk::tk_anomaly_diagnostics(month, overall) %>% filter(anomaly=='Yes')
