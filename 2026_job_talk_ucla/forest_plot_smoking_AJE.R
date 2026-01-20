library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# Create the data
res <- tibble::tribble(
  ~Effect, ~Risk_Difference, ~Risk_Ratio,
  "Total effect on dementia", "2.1 (-0.1, 4.2)", "1.21 (0.99, 1.50)",
  "Controlled direct effect on dementia", "-2.6 (-6.1, 0.8)", "0.86 (0.72, 1.05)",
  "Total effect on mortality", "-17.4 (-20.5, -14.2)", "0.68 (0.63, 0.72)"
) %>% 
  mutate(Effect = fct_rev(as.factor(Effect)))

# Function to extract estimates and CIs
extract_values <- function(x) {
  num <- as.numeric(str_extract(x, "[-]?[0-9]+\\.?[0-9]*"))
  lower <- as.numeric(str_extract(x, "(?<=\\().*?(?=,)"))
  upper <- as.numeric(str_extract(x, "(?<=, ).*?(?=\\))"))
  return(c(num, lower, upper))
}

# Process Risk Difference and Risk Ratio
res <- res %>%
  mutate(
    RD = map(Risk_Difference, extract_values),
    RR = map(Risk_Ratio, extract_values)
  ) %>%
  unnest_wider(RD, names_sep = "_") %>%
  unnest_wider(RR, names_sep = "_") %>%
  rename(RD_Estimate = RD_1, RD_Lower = RD_2, RD_Upper = RD_3,
         RR_Estimate = RR_1, RR_Lower = RR_2, RR_Upper = RR_3)

res <- res %>% 
  select(-2, -3) %>% 
pivot_longer( cols = -1,
              names_to = "metric", values_to = "value") %>% 
  separate(metric, into = c("risk", "estimate")) %>% 
  pivot_wider(names_from = "estimate",
              values_from = "value") %>% 
  mutate(ref_line = ifelse(risk == "RD", 0, 1)) %>%
  mutate(risk = ifelse(risk == "RD", "Risk Difference", "Risk Ratio"))


# Forest Plot

res %>%
  filter(Effect != "Total effect on mortality") %>% 
  ggplot(aes(x = Effect, y = Estimate, ymin = Lower, ymax = Upper, color = risk)) +
  geom_errorbar(width = 0.2, size = 1 ) +
  geom_point(size = 2, shape = 15) + 
  facet_wrap(.~ risk, scales = "free_x") +
  coord_flip() +
  geom_hline(aes(yintercept = ref_line),linetype = "dashed", color = "black") +
  labs(
    x = NULL,
    color = NULL,
    y = NULL
  ) +
  scale_color_manual(values = c("#DC267F", "#648FFF")) +
  theme_bw() +
  theme(
    legend.position = "none", 
    panel.spacing.x = unit(2, "lines"),
    axis.title.y = element_text(
      size = rel(2),
      margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(
      size = rel(2)),
    axis.title.x = element_text(
      size = rel(2),
      margin = margin(t = 20, r = 00, b = 0, l = 0)),
    legend.text = element_text(size = rel(2)),
    strip.text = element_text(size = rel(1.5)),
    axis.text.x = element_text(
      size = rel(2)))

