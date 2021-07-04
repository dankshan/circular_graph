library(tidyverse)
library(srvyr)
library(keyring)
library(openssl)

if (!("youth19_secret" %in% key_list()$service)) {
  key_set("youth19_secret")
}

svy <- cyphr::decrypt(
  readRDS("../Master data/data/svydesignCombinedCalibrated.rds"),
  cyphr::key_sodium(sha256(key_get_raw("youth19_secret")))
)

#set as srvyr survey for compatibility
svy <- as_survey_design(svy)


svy <-
  svy %>%
  mutate_at(vars(AccessHC), list(~if_else(Year == 2001, 0, .))) %>%
  mutate_at(vars(drivenAlc), list(~case_when(Year == 2001 ~ case_when(Injury9 %in% c(3,4,5) ~ 1,
                                                                      Injury9 %in% c(1,2) ~ 0,
                                                                      TRUE ~ .),
                                             
                                             Year == c(2007, 2012) ~ 0,
                                             TRUE ~ .)))

# thoughtSuicide
# drivenAlc


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



variables <- read_csv("./data/graph_variables_v2.csv") %>% na.omit()


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
      variable %in% c("AccessHC", "unableHC", "attemptSuicide", "wellbeing") ~ round((Year2019 - Year2007)/Year2007*100,2),
      TRUE ~ round((Year2019 - Year2001)/Year2001*100,2)),
  )

prevs2$variable <- fct_inorder(prevs2$variable)
prevs2$group <- fct_inorder(prevs2$group)

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

#preserve order of variables in graph to match groups


bands <-
  tibble(
    band0 = -90,
    band1 = 110,
    band2 = 115,
    band3 = 225,
    band4 = 270
  )




  prevs2 %>%
  ggplot(aes(x = variable, y = change)) +
  
  #background bars
  geom_bar(stat = "identity", fill = "white", colour = "#dadada", aes(x = variable, y = bands$band0), size = 1, inherit.aes = FALSE) +
  geom_bar(stat = "identity", fill = "white", colour = "#dadada", aes(x = variable, y = bands$band1), size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = c(0), colour = "grey20") +
  
  scale_color_manual(name = NULL, labels = c("Positive change", "No or minimal change", "Negative change"), values = c("91c017", "#9d9d9d", "#c90a0f")) +
  scale_fill_manual(name = NULL, labels = c("Positive change", "No or minimal change", "Negative change"), values = c("91c017", "#9d9d9d", "#c90a0f")) +
  labs(title = "Percentage change from 2001 to 2019",
       caption = "Note: * Change from 2007 to 2019. † Among sexual active students") +
  
  # scale_shape_manual(labels = c("Positive change", "No or minimal change", "Negative change"), values = c(21, 21, 21))+
  
  # group segments and text for groups and variables
  geom_rect(data = group_data, aes(xmin = start, ymin = bands$band2, xmax = end, ymax = bands$band3-5), fill = "#dadada", inherit.aes = FALSE) +
  geom_text(aes(x = variable, y = bands$band3-5 -(bands$band3-5 - bands$band2)/2, label = gsub("#", "\n", name)), size = 3) +
  
  geom_rect(data = group_data, aes(xmin = start, ymin = bands$band3, xmax = end, ymax = bands$band4), fill = "#0e70b7", inherit.aes = FALSE) +
  geom_text(data = group_data, aes(x = title_position, y = (bands$band4 - (bands$band4 - bands$band3)/2)-5, label = group), angle = group_data$angle2,  colour = "white", size = 4) +
  
  #change circle
  geom_point(aes(x = variable, y = 115, colour = colour), size = 10) + 
  geom_text(aes(x = variable, y = 115, label = round(change,0)), colour = "white", size = 4, fontface='bold') +
  
  #actual bars
  geom_bar(stat = "identity", width = 0.4, aes(fill = colour)) +
  
  ylim(-100, bands$band4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  ) +
  coord_polar(start = 0)

ggsave("./output/y19.jpg", height = 30, dpi = "retina", units = "cm")
