
#           Table with responsiveness per agency-halfyear     
# ======================================================
response.agency <- response2 %>% 
  group_by(response, agencyname) %>%
  mutate(response = case_when(
    response == 1 ~ "response",
    response == 0 ~ "no.response")) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  group_by(agencyname) %>%            # Group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  mutate(ratio = response/(response + no.response))

#           Plot
# ======================================================
response.plot <- response2 %>% 
  group_by(response, agencyname, day) %>%
  mutate(response = case_when(
    response == 1 ~ "response",
    response == 0 ~ "no.response")) %>%
  tally() %>% 
  tidyr::pivot_wider(names_from = response, 
                     values_from = n,
                     values_fill	= 0) %>%
  mutate(halfyear = as.Date(lubridate::floor_date(day, unit = "halfyear"))) %>%
  group_by(halfyear, agencyname) %>%            # Group by the binning variable
  summarise(no.response = sum(no.response),
            response = sum(response)) %>%
  group_by(agencyname) %>% 
  mutate(max = case_when(
    max(no.response) > max(response) ~ max(no.response),
    max(response) > max(no.response) ~ max(response)),
    response.rate = paste0(round(sum(response)/(sum(response) + sum(no.response)) * 100, 2), "%")) %>% 
  ungroup() %>%
  arrange(max) %>%               # Sort dataframe
  mutate(agencyname = factor(agencyname, unique(agencyname))) # Reset factor-column based on that order

# Creating a plot 
response.plot %>%
  ggplot(aes(x = halfyear)) +  
  facet_wrap(facets = . ~ agencyname,  nrow = 9, scales = "free_y") +
  geom_point(aes(y = (response + 1), color = "Response", shape = "Response"), size = 2) +
  geom_point(aes(y = (no.response + 1), color = "No response", shape = "No response"), size = 2) +
  geom_smooth(aes(y = (no.response + 1), color = "No response"), se = FALSE) +
  geom_smooth(aes(y = (response + 1), color = "Response"), se = FALSE) +
  scale_colour_manual("", values = c("#1aafd4", "#D43F1A")) +
  scale_shape_manual("", values = c(17, 16)) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%y") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black", size = 2),
        legend.key.size = unit(2, "line")) +
  labs(color  = "Guide name", shape = "Guide name") +
  xlab("Year") + ylab("Count (log)") + labs(title = "Responsiveness on Twitter per 6 months")
