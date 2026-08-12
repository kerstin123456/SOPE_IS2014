## Cognitive clusters and analytic datasets -------------------------------
source("R/00_setup.R")
obj <- readRDS(file.path(output_dir, "prepared_inputs.rds"))
list2env(obj, envir = environment())

## K-means profiles
X <- scale(cognit_cluster %>% select(f025r, log_speed, sd_speed))
set.seed(123)
km_final <- kmeans(X, centers = 3, nstart = 50)

cognit_cl <- cognit_cluster %>%
  mutate(cluster = km_final$cluster) %>%
  select(pid, syear, cluster)

cluster_2014 <- cognit_cl %>%
  transmute(pid, cluster_2014 = factor(cluster)) %>%
  distinct(pid, .keep_all = TRUE)

## Cross-sectional 2014 sample
analysis_2014 <- cognit_cl %>%
  left_join(pl_2014, by = c("pid", "syear")) %>%
  left_join(pgen_2014, by = c("pid", "syear")) %>%
  left_join(ppathl_demo, by = "pid") %>%
  left_join(
    cognit_cluster %>% select(pid, syear, f025r, log_speed, sd_speed),
    by = c("pid", "syear")
  ) %>%
  mutate(cluster = relevel(factor(cluster), ref = "2"))

## Longitudinal panel
panel_post2014 <- pl_panel %>%
  left_join(pgen_panel, by = c("pid", "syear")) %>%
  left_join(cluster_2014, by = "pid") %>%
  left_join(ppathl_demo, by = "pid") %>%
  mutate(
    life_sat = ifelse(plh0182 >= 0 & plh0182 <= 10, plh0182, NA_real_),
    health_original = ifelse(ple0008 >= 1 & ple0008 <= 5, ple0008, NA_real_),
    health_rev = ifelse(!is.na(health_original), 6 - health_original, NA_real_),
    age = ifelse(!is.na(gebjahr), syear - as.numeric(gebjahr), NA_real_),
    age = ifelse(age >= 0 & age <= 120, age, NA_real_)
  )

age_center <- mean(panel_post2014$age, na.rm = TRUE)
panel_post2014 <- panel_post2014 %>%
  mutate(
    age_c = age - age_center,
    cluster_2014 = relevel(factor(cluster_2014, levels = c("1", "2", "3")), ref = "2"),
    sex_f = factor(sex_f, levels = c("männlich", "weiblich"))
  )

if (anyDuplicated(panel_post2014[c("pid", "syear")])) {
  stop("Duplicate pid-syear rows detected in the merged longitudinal panel.")
}

## Main life-satisfaction REWB sample
## lag() denotes the previous observed wave, which need not be exactly one calendar year.
d_life_panel <- panel_post2014 %>%
  filter(!is.na(cluster_2014)) %>%
  arrange(pid, syear) %>%
  group_by(pid) %>%
  mutate(
    health_lag1 = lag(health_rev, order_by = syear),
    health_mean = mean(health_lag1, na.rm = TRUE),
    health_w = health_lag1 - health_mean
  ) %>%
  ungroup() %>%
  mutate(health_mean = ifelse(is.nan(health_mean), NA_real_, health_mean)) %>%
  filter(!is.na(life_sat), !is.na(health_lag1), !is.na(health_mean), !is.na(health_w)) %>%
  group_by(pid) %>%
  mutate(n_model_rows = n()) %>%
  ungroup() %>%
  filter(n_model_rows >= 2)

life_model_vars <- c(
  "life_sat", "health_w", "cluster_2014", "health_mean",
  "pgsbil", "age_c", "sex_f", "syear", "pid"
)

d_life_model <- d_life_panel %>%
  filter(if_all(all_of(life_model_vars), ~ !is.na(.x))) %>%
  group_by(pid) %>%
  mutate(n_complete_model_rows = n()) %>%
  ungroup() %>%
  filter(n_complete_model_rows >= 2) %>%
  mutate(
    cluster_2014 = droplevels(cluster_2014),
    sex_f = droplevels(sex_f)
  )

saveRDS(
  list(
    cognit_cl = cognit_cl,
    analysis_2014 = analysis_2014,
    panel_post2014 = panel_post2014,
    d_life_panel = d_life_panel,
    d_life_model = d_life_model,
    age_center = age_center
  ),
  file.path(output_dir, "analysis_datasets.rds")
)

write.csv(
  sample_summary(d_life_model, "Final life-satisfaction model"),
  file.path(output_dir, "main_sample_size.csv"),
  row.names = FALSE
)
