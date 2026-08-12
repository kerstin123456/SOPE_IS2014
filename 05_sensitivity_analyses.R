## Sensitivity analyses ----------------------------------------------------
source("R/00_setup.R")
inputs <- readRDS(file.path(output_dir, "prepared_inputs.rds"))
data_obj <- readRDS(file.path(output_dir, "analysis_datasets.rds"))
model_obj <- readRDS(file.path(output_dir, "primary_models.rds"))

pl <- inputs$pl
ppathl <- inputs$ppathl
d_life_model <- data_obj$d_life_model
m_life_rs <- model_obj$m_life_rs

key_terms <- c(
  "health_w",
  "health_w:cluster_20141",
  "health_w:cluster_20143"
)

extract_key <- function(model, label, n_persons, n_obs) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, conf.method = "Wald") %>%
    filter(term %in% key_terms) %>%
    transmute(
      Model = label,
      term,
      B = estimate,
      SE = std.error,
      CI_low = conf.low,
      CI_high = conf.high,
      Individuals = n_persons,
      Person_years = n_obs
    )
}

## Primary model rows for comparison
primary_results <- extract_key(
  m_life_rs,
  "Primary model",
  n_distinct(d_life_model$pid),
  nobs(m_life_rs)
)

## 1) Affect-adjusted sensitivity analysis
req_cols(pl, c("pid", "syear", "plh0184", "plh0185", "plh0186", "plh0187"), "pl")

affect_panel <- pl %>%
  select(pid, syear, plh0184, plh0185, plh0186, plh0187) %>%
  mutate(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    across(c(plh0184, plh0185, plh0186, plh0187), soep_to_na),
    neg_items_nonmiss = rowSums(!is.na(cbind(plh0184, plh0185, plh0187))),
    neg_affect_raw = rowMeans(cbind(plh0184, plh0185, plh0187), na.rm = TRUE),
    neg_affect = ifelse(neg_items_nonmiss >= 2, neg_affect_raw, NA_real_),
    pos_affect = plh0186
  ) %>%
  select(pid, syear, neg_affect, pos_affect)

if (anyDuplicated(affect_panel[c("pid", "syear")])) {
  stop("Duplicate pid-syear rows detected in affect data.")
}

d_life_affect_model <- d_life_model %>%
  left_join(affect_panel, by = c("pid", "syear")) %>%
  filter(!is.na(neg_affect), !is.na(pos_affect))

m_life_affect <- update(
  m_life_rs,
  . ~ . + neg_affect + pos_affect,
  data = d_life_affect_model
)

affect_results <- extract_key(
  m_life_affect,
  "Affect-adjusted model",
  n_distinct(d_life_affect_model$pid),
  nobs(m_life_affect)
)

## 2) Survey-weighted sensitivity analysis
req_cols(ppathl, c("pid", "syear", "phrf"), "ppathl")

weights_panel <- ppathl %>%
  transmute(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    phrf = soep_to_na(phrf)
  ) %>%
  distinct(pid, syear, .keep_all = TRUE)

d_life_weighted <- d_life_model %>%
  left_join(weights_panel, by = c("pid", "syear")) %>%
  filter(!is.na(phrf), phrf > 0) %>%
  mutate(phrf_std = phrf / mean(phrf, na.rm = TRUE))

weight_limits <- quantile(d_life_weighted$phrf_std, probs = c(.01, .99), na.rm = TRUE)

d_life_weighted <- d_life_weighted %>%
  mutate(
    phrf_trim = pmin(pmax(phrf_std, weight_limits[[1]]), weight_limits[[2]])
  )

m_life_weighted <- update(
  m_life_rs,
  data = d_life_weighted,
  weights = phrf_trim
)

weighted_results <- extract_key(
  m_life_weighted,
  "Weighted model",
  n_distinct(d_life_weighted$pid),
  nobs(m_life_weighted)
)

## Compact Supplementary Table S6 data
sensitivity_results <- bind_rows(primary_results, affect_results, weighted_results)
write.csv(sensitivity_results, file.path(output_dir, "table_S6_sensitivity_analyses.csv"), row.names = FALSE)

saveRDS(
  list(
    m_life_affect = m_life_affect,
    m_life_weighted = m_life_weighted,
    d_life_affect_model = d_life_affect_model,
    d_life_weighted = d_life_weighted
  ),
  file.path(output_dir, "sensitivity_models.rds")
)
