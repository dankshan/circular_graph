library(tidyverse)
library(srvyr)
library(keyring)
library(openssl)

if (!("youth19_secret" %in% key_list()$service)) {
  key_set("youth19_secret")
}


variables <- read_csv("./data/graph_variables.csv")

svy <- cyphr::decrypt(
  readRDS("../Master data/data/svydesignCombinedCalibrated.rds"),
  cyphr::key_sodium(sha256(key_get_raw("youth19_secret")))
)

#set as srvyr survey for compatibility
svy <- as_survey_design(svy)


svy <-
  svy %>%
  mutate_at(vars(AccessHC), list(~if_else(Year == 2001, 0, .)))


prev <- function(group, variable, colour, name) {
  
  svy %>%
    # filter(Year %in% c("2012", "2019")) %>%
    group_by(Year) %>%
    summarise(
      pct = survey_ratio(!!as.name(variable) == 1, !is.na(!!as.name(variable)), na.rm = TRUE, vartype = NULL)
    ) %>%
    mutate(
      variable = !!as.character(variable),
      group = !!as.character(group),
      colour = !!as.character(colour),
      name = !!as.character(name)
    ) %>%
    select(variable, everything())
  
}


prevs <- variables %>%
  pmap_df(
    function(var, group_name, change_colour, long_name) {
      prev(group = group_name, variable = var, colour = change_colour, name = long_name)
    }
  )

prevs2 <-
  prevs %>%
  pivot_wider(names_from = Year, values_from = pct, names_prefix = "Year") %>%
  mutate(
    variable = as.factor(variable),
    
    change = case_when(
      variable %in% c("AccessHC", "unableHC") ~ round(Year2019*100 - Year2007*100,2),
      TRUE ~ round(Year2019*100 - Year2001*100,2)),
  )


#where to position the grouping label above the graph
group_data <-
  prevs2 %>%
  arrange(group) %>%
  mutate(position = seq(1:nrow(prevs2))) %>%
  group_by(group) %>%
  summarise(start = min(position) - 0.46, end = max(position) + 0.46) %>%
  rowwise() %>%
  mutate(title_position = mean(c(start, end)),
         angle = 360 - ((360/nrow(prevs2) * title_position) - 360/nrow(prevs2)/2),
         angle2 = if_else(angle > 90 & angle < 270, angle + 180, angle)
  )

prevs2$variable <- fct_inorder(prevs2$variable)


prevs2 %>%
  ggplot(aes(x = variable, y = change)) +
  geom_bar(stat = "identity", fill = "white", colour = "#dadada", aes(x = variable, y = 50), size = 1, inherit.aes = FALSE) +
  geom_bar(stat = "identity", fill = "white", colour = "#dadada", aes(x = variable, y = -15), size = 1, inherit.aes = FALSE) +
  geom_segment(data = group_data, aes(x = start, y = 45, xend = end, yend = 45), size = 20, colour = "#0e70b7", inherit.aes = FALSE) +
  geom_segment(data = group_data, aes(x = start, y = 30, xend = end, yend = 30), size = 40, colour = "#dadada", inherit.aes = FALSE) +
  geom_hline(yintercept = c(0), colour = "grey20") +
  scale_color_manual(labels = c("Positive change", "No or minimal change", "Negative change"), values = c("91c017", "#9d9d9d", "#c90a0f")) +
  scale_fill_manual(labels = c("Positive change", "No or minimal change", "Negative change"), values = c("91c017", "#9d9d9d", "#c90a0f")) +
  geom_bar(stat = "identity", width = 0.4, aes(fill = colour)) +
  geom_point(aes(x = variable, y = 20, colour = colour), size = 10) + 
  geom_text(data = group_data, aes(x = title_position, y = 45, label = group), angle = group_data$angle2,  colour = "white", size = 4, fontface='bold') +
  geom_text(aes(x = variable, y = 20, label = round(change,0)), colour = "white", size = 4, fontface='bold') +
  geom_text(aes(x = variable, y = 30, label = gsub("#", "\n", name)), size = 3) +
  ylim(-20, 50) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_polar(start = 0)

