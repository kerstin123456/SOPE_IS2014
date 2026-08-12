## Exploratory between-person analyses, 2014 ------------------------------
source("R/00_setup.R")
obj <- readRDS(file.path(output_dir, "analysis_datasets.rds"))
analysis_2014 <- obj$analysis_2014

required_variables <- c(
  "pid", "cluster", "life_satisfaction", "self_rated_health",
  "neg_affect", "pos_affect", "pgsbil", "age_2014", "sex_f"
)
req_cols(analysis_2014, required_variables, "analysis_2014")

d_exploratory <- analysis_2014 %>%
  mutate(
    cluster = factor(cluster, levels = c("2", "1", "3")),
    sex_f = droplevels(factor(sex_f))
  )

fit_exploratory_model <- function(data, outcome, outcome_label) {
  model_vars <- c(outcome, "cluster", "pgsbil", "age_2014", "sex_f")
  model_data <- data %>%
    select(pid, all_of(model_vars)) %>%
    filter(if_all(all_of(model_vars), ~ !is.na(.x))) %>%
    mutate(cluster = droplevels(cluster), sex_f = droplevels(sex_f))

  f <- reformulate(c("cluster", "pgsbil", "age_2014", "sex_f"), response = outcome)
  m <- lm(f, data = model_data)

  list(
    model = m,
    data = model_data,
    tidy = broom::tidy(m, conf.int = TRUE, conf.level = .95) %>%
      mutate(outcome_label = outcome_label, n = nrow(model_data))
  )
}

models <- list(
  fit_exploratory_model(d_exploratory, "life_satisfaction", "Life satisfaction"),
  fit_exploratory_model(d_exploratory, "self_rated_health", "Self-rated health"),
  fit_exploratory_model(d_exploratory, "neg_affect", "Negative affect"),
  fit_exploratory_model(d_exploratory, "pos_affect", "Positive affect")
)

cluster_results <- bind_rows(lapply(models, `[[`, "tidy")) %>%
  filter(term %in% c("cluster1", "cluster3")) %>%
  mutate(
    Comparison = recode(
      term,
      cluster1 = "Cluster 1 vs. Cluster 2",
      cluster3 = "Cluster 3 vs. Cluster 2"
    )
  ) %>%
  transmute(
    Outcome = outcome_label,
    Comparison,
    B = estimate,
    SE = std.error,
    CI_low = conf.low,
    CI_high = conf.high,
    p = p.value,
    N = n
  )

## Descriptive statistics by cognitive cluster
descriptive_table <- d_exploratory %>%
  group_by(cluster) %>%
  summarise(
    n = n_distinct(pid),
    life_mean = mean(life_satisfaction, na.rm = TRUE),
    life_sd = sd(life_satisfaction, na.rm = TRUE),
    health_mean = mean(self_rated_health, na.rm = TRUE),
    health_sd = sd(self_rated_health, na.rm = TRUE),
    negative_affect_mean = mean(neg_affect, na.rm = TRUE),
    negative_affect_sd = sd(neg_affect, na.rm = TRUE),
    positive_affect_mean = mean(pos_affect, na.rm = TRUE),
    positive_affect_sd = sd(pos_affect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Cluster = recode(
      as.character(cluster),
      `1` = "Cluster 1",
      `2` = "Cluster 2 (reference)",
      `3` = "Cluster 3"
    )
  ) %>%
  select(Cluster, everything(), -cluster)

write.csv(cluster_results, file.path(output_dir, "table_S5_exploratory_regressions.csv"), row.names = FALSE)
write.csv(descriptive_table, file.path(output_dir, "descriptive_statistics_by_cluster.csv"), row.names = FALSE)
saveRDS(models, file.path(output_dir, "exploratory_models.rds"))
