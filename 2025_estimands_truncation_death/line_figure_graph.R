library(tidyverse)

data_no_cens <- tibble::tribble(
  ~id, ~time,      ~Treatment,           ~Event,
  1L,    12L,      "Intervention",            NA,
  2L,    6L,      "Intervention",  "Hospitalization",
  3L,   12L,      "Intervention",            NA,
  4L,    12L,      "Intervention",            NA,
  5L,   12L,      "Intervention",            NA,
  6L,    8L, "Usual care",  "Hospitalization",
  7L,    12L, "Usual care",  NA,
  8L,   10L, "Usual care",            "Hospitalization",
  9L,    12L, "Usual care",            NA,
  10L,    4L, "Usual care",       "Hospitalization"
)

fig_no_cens <- data_no_cens  %>% 
  mutate(
    id = fct_rev(as_factor(id)),  # Reverse order to match top-down ID
    treatment = as_factor(Treatment)
  ) %>%
  ggplot(aes(x = id, y = time, color = Treatment)) +
  geom_segment(aes(xend = id, y = 0, yend = time), size = 1) +  # thicker line
  geom_point(aes(shape = Event), size = 3) +
  coord_flip() +
  scale_color_manual(values = c("#2a7f94", "#e4a803")) +
  scale_shape_manual(values = c(Hospitalization = 15), na.translate = FALSE) +  
  scale_y_continuous(breaks = seq(0, 12, 1), expand = c(0,0), limits = c(0,13)) +
  theme_minimal() +
  theme(legend.text = element_text(size=12)) +
  theme(strip.text.x = element_text(size = 11),
        strip.background = element_rect(fill=NA),
        axis.text=element_text(size=12)) +
  theme(
    axis.line.x = element_line(colour = "gray30", 
                               size=1, 
                               lineend = "butt"),
    axis.line.y = element_line(colour = "gray30", 
                               size=1)) +
  labs(
    y = "Months of follow-up",
    x = "ID",
    shape = "",
    color = ""
  ) 

ggsave("line_graph_no_cens.png", plot = fig_no_cens, 
       path = here::here("2025_causal_survival","figs"),
       width = 6, height = 4, dpi = 600)

# With loss to follow-up --------------------------------------------------

data_cens <- tibble::tribble(
  ~id, ~time,      ~Treatment,           ~Event,
  1L,    3L,      "Intervention",            "LTFU",
  2L,    6L,      "Intervention",  "Hospitalization",
  3L,   5L,      "Intervention",            "LTFU",
  4L,    8L,      "Intervention",           "LTFU",
  5L,   12L,      "Intervention",            NA,
  6L,    8L, "Usual care",  "Hospitalization",
  7L,    6L, "Usual care",  "LTFU",
  8L,   10L, "Usual care",            "Hospitalization",
  9L,    12L, "Usual care",            NA,
  10L,    4L, "Usual care",       "Hospitalization"
)

data_cens %>% 
  mutate(
    id = fct_rev(as_factor(id)),  # Reverse order to match top-down ID
    treatment = as_factor(Treatment)
  ) -> data_cens_proc

fig_cens <- data_cens_proc %>% 
  ggplot(aes(x = id, y = time, color = Treatment)) +
  # Observed follow-up time
  geom_segment(aes(xend = id, y = 0, yend = time), size = 1) +
  # Dotted lines for LTFU extending to 12
  geom_segment(
    data = filter(data_cens_proc, Event == "LTFU" & time < 12),
    aes(xend = id, y = time, yend = 12, linetype = "Unobserved outcome"),
    size = 1,
    inherit.aes = TRUE,
    show.legend = TRUE
  ) +
  # Event points
  geom_point(aes(shape = Event), size = 3) +
  coord_flip() +
  scale_color_manual(values = c("#2a7f94", "#e4a803")) +
  scale_shape_manual(values = c(Hospitalization = 15, LTFU = 7), na.translate = FALSE) +
  scale_linetype_manual(name = "", values = c("Unobserved outcome" = "dotted")) +
  scale_y_continuous(breaks = seq(0, 12, 1), expand = c(0,0), limits = c(0,13)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size=12),
    strip.text.x = element_text(size = 11),
    strip.background = element_rect(fill=NA),
    axis.text = element_text(size=12),
    axis.line.x = element_line(colour = "gray30", size=1, lineend = "butt"),
    axis.line.y = element_line(colour = "gray30", size=1)
  ) +
  labs(
    y = "Months of follow-up",
    x = "ID",
    shape = "",
    color = ""
  )

ggsave("line_graph_cens.png", plot = fig_cens, 
       path = here::here("2025_causal_survival","figs"),
       width = 6, height = 4, dpi = 600)


# With death as competing events ------------------------------------------

data_comp_event <- tibble::tribble(
  ~id, ~time,      ~Treatment,           ~Event,
  1L,    3L,      "Intervention",            "Death",
  2L,    6L,      "Intervention",  "Hospitalization",
  3L,   5L,      "Intervention",            "Death",
  4L,    8L,      "Intervention",           "LTFU",
  5L,   12L,      "Intervention",            NA,
  6L,    8L, "Usual care",  "Hospitalization",
  7L,    6L, "Usual care",  "Death",
  8L,   10L, "Usual care",            "Hospitalization",
  9L,    12L, "Usual care",            NA,
  10L,    4L, "Usual care",       "Hospitalization"
)
data_comp_event %>%
  mutate(
    id = fct_rev(as_factor(id)),
    Treatment = as_factor(Treatment)
  ) -> data_comp_event

fig_comp_events <- data_comp_event %>% 
  ggplot(aes(x = id, y = time, color = Treatment)) +
  # geom_segment(
  #   data = filter(data_comp_event, Event == "LTFU" & time < 12),
  #   aes(x = id, xend = id, y = time, yend = 12, linetype = "Unobserved outcome"),
  #   size = 1,
  #   inherit.aes = FALSE,
  #   show.legend = TRUE
  # ) +
  geom_segment(aes(xend = id, y = 0, yend = time), size = 1) +
  # Event points
  geom_point(aes(shape = Event), size = 3) +
  coord_flip() +
  scale_color_manual(values = c("#2a7f94", "#e4a803")) +
  scale_shape_manual(values = c(Hospitalization = 15, LTFU = 7, Death = 8), na.translate = FALSE) +
  scale_linetype_manual(name = "", values = c("Unobserved outcome" = "dotted")) +
  scale_y_continuous(breaks = seq(0, 12, 1), expand = c(0,0), limits = c(0,13)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 11),
    strip.background = element_rect(fill = NA),
    axis.text = element_text(size = 12),
    axis.line.x = element_line(colour = "gray30", size = 1, lineend = "butt"),
    axis.line.y = element_line(colour = "gray30", size = 1)
  ) +
  labs(
    y = "Months of follow-up",
    x = "ID",
    shape = "",
    color = ""
  )


ggsave("line_graph_comp_events.png", plot = fig_comp_events, 
       path = here::here("2025_causal_survival","figs"),
       width = 6, height = 4, dpi = 600)
