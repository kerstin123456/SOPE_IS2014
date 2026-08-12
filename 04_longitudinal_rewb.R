## Primary longitudinal REWB models ---------------------------------------
source("R/00_setup.R")
obj <- readRDS(file.path(output_dir, "analysis_datasets.rds"))
d_life_model <- obj$d_life_model

f_life_ri <- life_sat ~
  health_w * cluster_2014 + health_mean + pgsbil + age_c + sex_f +
  factor(syear) + (1 | pid)

f_life_rs <- life_sat ~
  health_w * cluster_2014 + health_mean + pgsbil + age_c + sex_f +
  factor(syear) + (1 + health_w | pid)

m_life_ri <- lmer(
  f_life_ri, data = d_life_model, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

m_life_rs <- lmer(
  f_life_rs, data = d_life_model, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

## Fixed effects, Wald CIs and normal-approximation p values
extract_fixed <- function(model, model_name) {
  out <- broom.mixed::tidy(
    model, effects = "fixed", conf.int = TRUE,
    conf.level = .95, conf.method = "Wald"
  )
  out$p.value <- 2 * pnorm(abs(out$statistic), lower.tail = FALSE)
  out %>% mutate(Model = model_name)
}

fixed_effects <- bind_rows(
  extract_fixed(m_life_ri, "Random-intercept model"),
  extract_fixed(m_life_rs, "Random-slope model")
)

model_comparison <- as.data.frame(anova(m_life_ri, m_life_rs)) %>%
  rownames_to_column("Model")

random_effects <- as.data.frame(VarCorr(m_life_rs))

## Profile-likelihood CI for random-effects SD/correlation parameters
profile_ci <- confint(
  m_life_rs,
  parm = "theta_",
  method = "profile",
  oldNames = FALSE
) %>%
  as.data.frame() %>%
  rownames_to_column("Parameter")

## Cluster-specific simple slopes and Bonferroni contrasts
simple_slopes <- emtrends(m_life_rs, specs = ~ cluster_2014, var = "health_w")
simple_slopes_table <- as.data.frame(summary(simple_slopes, infer = c(TRUE, TRUE)))
slope_contrasts_table <- as.data.frame(
  summary(pairs(simple_slopes, adjust = "bonferroni"), infer = c(TRUE, TRUE))
)

saveRDS(
  list(m_life_ri = m_life_ri, m_life_rs = m_life_rs),
  file.path(output_dir, "primary_models.rds")
)
write.csv(fixed_effects, file.path(output_dir, "main_REWB_fixed_effects.csv"), row.names = FALSE)
write.csv(model_comparison, file.path(output_dir, "main_REWB_model_comparison.csv"), row.names = FALSE)
write.csv(random_effects, file.path(output_dir, "main_REWB_random_effects.csv"), row.names = FALSE)
write.csv(profile_ci, file.path(output_dir, "main_REWB_profile_CI.csv"), row.names = FALSE)
write.csv(simple_slopes_table, file.path(output_dir, "simple_slopes.csv"), row.names = FALSE)
write.csv(slope_contrasts_table, file.path(output_dir, "simple_slope_contrasts.csv"), row.names = FALSE)
