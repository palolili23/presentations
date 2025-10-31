install.packages("waffle")
library(waffle)

library(tidyverse)
library(kableExtra)

risk_waffle_plot <- function(data, ...) {
  data %>% 
    ggplot() +
    geom_waffle(
      aes(fill = outcome, values = count),
      color = "white",
      n_rows = 4,
      size = .5,
    ) +
    facet_grid(. ~ year_n, switch = "x") +
    ggthemes::scale_fill_tableau(name=NULL) +
    coord_equal() +
    theme_minimal(base_family = "Roboto Condensed") +
    theme_enhance_waffle() +
    theme(plot.caption = element_text(hjust = 0, face= "italic")) +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0))}

## Data

data <- tibble::tribble(
  ~year, ~total, ~dementia, ~died, ~alive, ~dementia_cum, ~died_cum, ~alive_cum,
  0,    20,        0,    0,    20,            0,        0,        20,
  1,    20,        0,    3,    17,            0,        1,        19,
  2,    17,        1,    1,    15,            1,        1,        18,
  3,    15,        1,    2,    12,            2,        3,        15,
  4,    12,        2,    1,    9,            4,        4,        12,
  5,    9,        0,    3,    6,            4,        6,        10,
  6,    6,        3,    0,     3,            7,        6,         7 )

data %>% 
  kable() %>% 
  kable_styling()

data %>% 
  select(year, total, dementia, died, alive) %>% 
  pivot_longer(
    cols = c(3:5),
    names_to = "outcome",
    values_to = "count") %>% 
  mutate(year_n = paste0("Year ", year, "\n n = ", total),
         outcome = str_to_title(outcome)) %>% 
  risk_waffle_plot() +
  labs(
    title = "How data looks like",
    x = "Year",
    y = "Count")

data %>% 
  select(year, total, dementia) %>% 
  mutate(alive = total - dementia,
         hz = paste0(dementia, "/", total)) %>% 
  pivot_longer(
    cols = -c(1,2,5),
    names_to = "outcome",
    values_to = "count") %>% 
  mutate(year_n = paste0("Year ", year, "\n n = ", total,
                         "\n\n ", hz),
         outcome = str_to_title(outcome)) %>% 
  risk_waffle_plot() +
  labs(
    title = "Cause-specific hazard",
    x = "Year",
    y = "Count",
    caption = "\n\n Conditional probability of not having the event or the competing event at the previous time-point"
  ) 

data %>% 
  select(year, total, dementia) %>% 
  mutate(alive = total - dementia,
         hz = paste0(alive, "/", total)) %>% 
  pivot_longer(
    cols = -c(1,2,5),
    names_to = "outcome",
    values_to = "count") %>% 
  mutate(year_n = paste0("Year ", year, "\n n = ", total,
                         "\n\n ", hz),
         outcome = str_to_title(outcome),
         outcome = fct_rev(outcome)) %>% 
  risk_waffle_plot() +
  labs(
    title = "Kaplan-Meier Method",
    x = "Year",
    y = "Count",
    caption = "\n\n Conditional probability of not having the event per year"
  ) 

data %>% 
  select(year, total, dementia, died, alive, dementia_cum) %>% 
  mutate(alive = 20 - (dementia_cum),
         alive = lag(alive, default = 20),
         alive = alive - dementia,
         total = alive + dementia,
         sdhz = paste0(dementia, "/", total)) %>% 
  select(year, total, sdhz, dementia, alive) %>% 
  pivot_longer(
    cols = -c(1:3),
    names_to = "outcome",
    values_to = "count") %>% 
  mutate(year_n = paste0("Year ", year, "\n n = ", total,
                         "\n\n ", sdhz),
         outcome = ifelse(outcome == "alive", "alive or death", outcome),
         outcome = str_to_title(outcome)
  ) %>% 
  risk_waffle_plot() +
  labs(
    title = "Sub-distribution hazard",
    x = "Year",
    y = "Count",
    caption = "\n\n Conditional of not having the event at the previous time-point")

```