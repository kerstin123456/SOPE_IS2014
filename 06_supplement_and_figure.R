## Supplementary diagnostics and Figure 2 -------------------------------
source("R/00_setup.R")
data_obj <- readRDS(file.path(output_dir, "analysis_datasets.rds"))
model_obj <- readRDS(file.path(output_dir, "primary_models.rds"))

cognit_cl <- data_obj$cognit_cl
analysis_2014 <- data_obj$analysis_2014
panel_post2014 <- data_obj$panel_post2014
d_life_panel <- data_obj$d_life_panel
d_life_model <- data_obj$d_life_model
m_life_rs <- model_obj$m_life_rs

## Sample flow used for the longitudinal life-satisfaction analysis
sample_flow <- bind_rows(
  tibble(
    Step = "Valid 2014 cognitive profile",
    Individuals = n_distinct(cognit_cl$pid),
    Person_years = NA_integer_
  ),
  tibble(
    Step = "Broad post-2014 panel, 2014-2023",
    Individuals = n_distinct(panel_post2014$pid),
    Person_years = nrow(panel_post2014)
  ),
  tibble(
    Step = "Post-2014 observations linked to a cognitive profile",
    Individuals = panel_post2014 %>% filter(!is.na(cluster_2014)) %>% summarise(n = n_distinct(pid)) %>% pull(n),
    Person_years = sum(!is.na(panel_post2014$cluster_2014))
  ),
  tibble(
    Step = "At least two model-eligible life-satisfaction observations",
    Individuals = n_distinct(d_life_panel$pid),
    Person_years = nrow(d_life_panel)
  ),
  tibble(
    Step = "Final complete-case life-satisfaction model",
    Individuals = n_distinct(d_life_model$pid),
    Person_years = nrow(d_life_model)
  )
) %>%
  mutate(Excluded_from_previous = c(NA_integer_, NA_integer_, 0L, -diff(Individuals)[3:4]))

## Variable-level missingness
missingness_2014 <- tibble(
  Variable = c(
    "Word-test score", "Mean log reaction time", "Reaction-time variability",
    "Life satisfaction", "Self-rated health", "Negative affect", "Positive affect",
    "Education", "Age", "Sex"
  ),
  N_total = nrow(analysis_2014),
  N_nonmissing = c(
    sum(!is.na(analysis_2014$f025r)),
    sum(!is.na(analysis_2014$log_speed)),
    sum(!is.na(analysis_2014$sd_speed)),
    sum(!is.na(analysis_2014$life_satisfaction)),
    sum(!is.na(analysis_2014$self_rated_health)),
    sum(!is.na(analysis_2014$neg_affect)),
    sum(!is.na(analysis_2014$pos_affect)),
    sum(!is.na(analysis_2014$pgsbil)),
    sum(!is.na(analysis_2014$age_2014)),
    sum(!is.na(analysis_2014$sex_f))
  )
) %>%
  mutate(
    N_missing = N_total - N_nonmissing,
    Missing_percent = 100 * N_missing / N_total
  )

missingness_post2014 <- tibble(
  Variable = c("Cognitive profile available", "Life satisfaction", "Self-rated health", "Education", "Age", "Sex"),
  Person_years = nrow(panel_post2014),
  Available = c(
    sum(!is.na(panel_post2014$cluster_2014)),
    sum(!is.na(panel_post2014$life_sat)),
    sum(!is.na(panel_post2014$health_rev)),
    sum(!is.na(panel_post2014$pgsbil)),
    sum(!is.na(panel_post2014$age_c)),
    sum(!is.na(panel_post2014$sex_f))
  )
) %>%
  mutate(
    Unavailable_or_missing = Person_years - Available,
    Percent = 100 * Unavailable_or_missing / Person_years
  )

## Included-versus-excluded baseline characteristics
final_ids <- d_life_model %>% distinct(pid) %>% mutate(included = TRUE)
selection_person <- analysis_2014 %>%
  distinct(pid, .keep_all = TRUE) %>%
  left_join(final_ids, by = "pid") %>%
  mutate(included = ifelse(is.na(included), FALSE, included))

continuous_vars <- c(
  "age_2014", "pgsbil", "life_satisfaction", "self_rated_health",
  "neg_affect", "pos_affect", "f025r", "log_speed", "sd_speed"
)

compare_continuous <- function(v) {
  x1 <- selection_person[[v]][selection_person$included & !is.na(selection_person[[v]])]
  x0 <- selection_person[[v]][!selection_person$included & !is.na(selection_person[[v]])]
  pooled <- sqrt(((length(x1)-1)*var(x1) + (length(x0)-1)*var(x0)) / (length(x1)+length(x0)-2))
  test <- t.test(x1, x0)
  tibble(
    Variable = v,
    Included_n = length(x1), Included_M = mean(x1), Included_SD = sd(x1),
    Excluded_n = length(x0), Excluded_M = mean(x0), Excluded_SD = sd(x0),
    SMD = (mean(x1)-mean(x0))/pooled,
    p = test$p.value
  )
}

selection_continuous <- bind_rows(lapply(continuous_vars, compare_continuous))

## Figure 2: primary unweighted model
health_values <- seq(
  quantile(d_life_model$health_w, .01, na.rm = TRUE),
  quantile(d_life_model$health_w, .99, na.rm = TRUE),
  length.out = 60
)

emm_plot <- emmeans(
  m_life_rs,
  specs = ~ cluster_2014 | health_w,
  at = list(
    health_w = health_values,
    health_mean = mean(d_life_model$health_mean, na.rm = TRUE),
    pgsbil = mean(d_life_model$pgsbil, na.rm = TRUE),
    age_c = 0
  ),
  nuisance = c("syear", "sex_f"),
  weights = "proportional"
)

plot_data <- as.data.frame(summary(emm_plot, infer = c(TRUE, FALSE)))
if (all(c("lower.CL", "upper.CL") %in% names(plot_data))) {
  plot_data <- rename(plot_data, ci_lower = lower.CL, ci_upper = upper.CL)
} else {
  plot_data <- rename(plot_data, ci_lower = asymp.LCL, ci_upper = asymp.UCL)
}
plot_data <- plot_data %>%
  mutate(profile = factor(
    cluster_2014,
    levels = c("1", "2", "3"),
    labels = c("Cluster 1", "Cluster 2 (reference)", "Cluster 3")
  ))

p <- ggplot(plot_data, aes(health_w, emmean, group = profile, linetype = profile)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = profile), alpha = .12, colour = NA) +
  geom_line(linewidth = 1.05) +
  labs(
    x = "Within-person deviation from average self-rated health",
    y = "Predicted life satisfaction",
    linetype = "Cognitive profile",
    fill = "Cognitive profile"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Figure_2_primary_REWB.png"), p, width = 7.2, height = 5.2, dpi = 300)

write.csv(sample_flow, file.path(output_dir, "table_S1_sample_flow.csv"), row.names = FALSE)
write.csv(missingness_2014, file.path(output_dir, "table_S2_missingness_2014.csv"), row.names = FALSE)
write.csv(missingness_post2014, file.path(output_dir, "table_S3_missingness_post2014.csv"), row.names = FALSE)
write.csv(selection_continuous, file.path(output_dir, "table_S4_included_excluded_continuous.csv"), row.names = FALSE)
