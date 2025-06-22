library(dplyr)
library(survival)
library(survminer)

set.seed(123)  # for reproducibility

# Number of observations
n <- 2000

# Simulate Treatment groups (half Intervention, half Usual care)
data_sim <- tibble(
  id = 1:n,
  Treatment = rep(c("Intervention", "Usual care"), each = n/2)
)

# Simulate survival times
# Intervention: longer survival on average
# Usual care: shorter survival on average

data_sim <- tibble(
  id = 1:n,
  Treatment = rep(c("Intervention", "Usual care"), each = n/2)
) %>%
  mutate(
    # Usual care has a higher rate in earlier months
    time = ifelse(
      Treatment == "Intervention",
      rexp(n/2, rate = 0.08),  # Lower rate for Intervention
      ifelse(
        runif(n/2) < 0.5,  # High event rate in first 6 months
        rexp(n/2, rate = 0.2),  # High rate in early months
        rexp(n/2, rate = 0.1)   # Lower rate later
      )
    ),
    admin_censor = 12,
    status = ifelse(time <= admin_censor, 1, 0),    
    time = pmin(time, admin_censor)
  )


# Set administrative censoring at 12 months
admin_censor_time <- 12

data_sim <- data_sim %>%
  mutate(
    status = ifelse(time <= admin_censor_time, 1, 0),   # 1=event, 0=censored
    time = pmin(time, admin_censor_time)
  )

# View first few rows
head(data_sim)

# Create a survival object
surv_obj <- Surv(time = data_sim$time, event = data_sim$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Treatment, data = data_sim)

# Plot survival curves
ggsurvplot(
  fit, 
  data = data_sim,
  conf.int = TRUE,
  risk.table = TRUE,
  fun = "event",
  ggtheme = theme_minimal(),
  palette = c("#2a7f94", "#e4a803"),
  legend.title = "",
  surv.median.line = "hv",
  risk.table.y.text.col = TRUE
)




# hazard ------------------------------------------------------------------
library(dplyr)
library(survival)
library(bshazard)
library(ggplot2)

set.seed(123)

# Number of observations
n <- 2000

# Simulate Treatment groups
data_sim <- tibble(
  id = 1:n,
  Treatment = rep(c("Intervention", "Usual care"), each = n/2)
) %>%
  mutate(
    # Usual care has a higher rate in earlier months
    time = ifelse(
      Treatment == "Intervention",
      rexp(n/2, rate = 0.08),  # Lower rate for Intervention
      ifelse(
        runif(n/2) < 0.5,  # High event rate in first 6 months
        rexp(n/2, rate = 0.2),  # High rate in early months
        rexp(n/2, rate = 0.1)   # Lower rate later
      )
    ),
    admin_censor = 12,
    status = ifelse(time <= admin_censor, 1, 0),    
    time = pmin(time, admin_censor)
  )

# Estimate smoothed hazard function for each group separately
haz_int <- bshazard(Surv(time, status) ~ 1, data = data_sim %>% filter(Treatment == "Intervention"))
haz_usual <- bshazard(Surv(time, status) ~ 1, data = data_sim %>% filter(Treatment == "Usual care"))

# Extract hazard estimates into data frames
haz_int_df <- data.frame(
  time = haz_int$time,
  hazard = haz_int$hazard,
  lower = haz_int$lower,
  upper = haz_int$upper,
  Treatment = "Intervention"
)

haz_usual_df <- data.frame(
  time = haz_usual$time,
  hazard = haz_usual$hazard,
  lower = haz_usual$lower,
  upper = haz_usual$upper,
  Treatment = "Usual care"
)

# Combine both hazard estimates
haz_all <- bind_rows(haz_int_df, haz_usual_df)

# Plot the hazard functions
ggplot(haz_all, aes(x = time, y = hazard, color = Treatment, fill = Treatment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("#2a7f94", "#e4a803")) +
  scale_fill_manual(values = c("#2a7f94", "#e4a803")) +
  facet_wrap(~Treatment, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +  
  theme_minimal() +
  labs(
    title = "Instantaneous Hazard Function",
    x = "Time (months)",
    y = "Estimated Hazard"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )
