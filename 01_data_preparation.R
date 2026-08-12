## Data preparation --------------------------------------------------------
source("R/00_setup.R")

## Load raw files
pl <- read_soep_csv(pl_path)
pgen <- read_soep_csv(pgen_path)
ppathl <- haven::read_sav(ppathl_path)
cognit <- haven::read_sav(cognit_path)

req_cols(pl, c("pid", "syear", "plh0182", "ple0008"), "pl")
req_cols(pgen, c("pid", "syear", "pgsbil"), "pgen")
req_cols(ppathl, c("pid", "syear", "sex", "gebjahr", "phrf"), "ppathl")
req_cols(cognit, c("pid", "syear", "f025r"), "cognit")

## Cognitive assessment: 2014 only
cognit <- cognit %>%
  mutate(pid = as.numeric(pid), syear = as.integer(syear)) %>%
  filter(syear == 2014)

cognit$f025r <- soep_to_na(cognit$f025r)
f099_times <- grep("^f099time", names(cognit), value = TRUE)
if (length(f099_times) == 0) stop("No f099time reaction-time variables found in cognit.sav")
for (v in f099_times) cognit[[v]] <- soep_to_na(cognit[[v]])

cognit <- cognit %>%
  mutate(
    n_speed_items = rowSums(!is.na(pick(all_of(f099_times)))),
    mean_speed = rowMeans(pick(all_of(f099_times)), na.rm = TRUE),
    sd_speed = apply(pick(all_of(f099_times)), 1, sd, na.rm = TRUE),
    mean_speed = ifelse(n_speed_items > 0, mean_speed, NA_real_),
    sd_speed = ifelse(n_speed_items >= 2, sd_speed, NA_real_),
    log_speed = ifelse(mean_speed > 0, log(mean_speed), NA_real_)
  )

cognit_cluster <- cognit %>%
  select(pid, syear, f025r, log_speed, sd_speed) %>%
  filter(!is.na(pid), !is.na(f025r), !is.na(log_speed), !is.na(sd_speed))

## 2014 outcomes and affect
pl_keep_2014 <- c(
  "pid", "syear", "plh0182", "ple0008",
  "plh0184", "plh0185", "plh0186", "plh0187"
)
req_cols(pl, pl_keep_2014, "pl")

pl_2014 <- pl %>%
  select(all_of(pl_keep_2014)) %>%
  mutate(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    across(-c(pid, syear), soep_to_na)
  ) %>%
  filter(syear == 2014) %>%
  mutate(
    life_satisfaction = ifelse(plh0182 >= 0 & plh0182 <= 10, plh0182, NA_real_),
    self_rated_health = ifelse(ple0008 >= 1 & ple0008 <= 5, 6 - ple0008, NA_real_),
    neg_items_nonmiss = rowSums(!is.na(cbind(plh0184, plh0185, plh0187))),
    neg_affect_raw = rowMeans(cbind(plh0184, plh0185, plh0187), na.rm = TRUE),
    neg_affect = ifelse(neg_items_nonmiss >= 2, neg_affect_raw, NA_real_),
    pos_affect = plh0186
  )

## 2014 education
pgen_2014 <- pgen %>%
  select(pid, syear, pgsbil) %>%
  mutate(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    pgsbil = soep_to_na(pgsbil)
  ) %>%
  filter(syear == 2014)

## Stable demographics. ppathl is retained separately because phrf is wave-specific.
ppathl_demo <- ppathl %>%
  transmute(
    pid = as.numeric(pid),
    sex = soep_to_na(sex),
    gebjahr = soep_to_na(gebjahr)
  ) %>%
  distinct(pid, .keep_all = TRUE) %>%
  mutate(
    age_2014 = 2014L - as.integer(gebjahr),
    age_2014 = ifelse(age_2014 >= 0 & age_2014 <= 120, age_2014, NA_integer_),
    sex_f = factor(sex, levels = c(1, 2), labels = c("männlich", "weiblich"))
  )

## Longitudinal PL and PGEN data are restricted BEFORE lags/person means.
pl_panel <- pl %>%
  select(pid, syear, plh0182, ple0008) %>%
  mutate(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    plh0182 = soep_to_na(plh0182),
    ple0008 = soep_to_na(ple0008)
  ) %>%
  filter(!is.na(pid), !is.na(syear), syear >= 2014)

if (anyDuplicated(pl_panel[c("pid", "syear")])) {
  stop("Duplicate pid-syear rows detected in PL after the 2014 restriction.")
}

pgen_panel <- pgen %>%
  select(pid, syear, pgsbil) %>%
  mutate(
    pid = as.numeric(pid),
    syear = as.integer(syear),
    pgsbil = soep_to_na(pgsbil)
  ) %>%
  filter(!is.na(pid), !is.na(syear), syear >= 2014)

if (anyDuplicated(pgen_panel[c("pid", "syear")])) {
  stop("Duplicate pid-syear rows detected in PGEN after the 2014 restriction.")
}

saveRDS(
  list(
    pl = pl,
    pgen = pgen,
    ppathl = ppathl,
    cognit_cluster = cognit_cluster,
    pl_2014 = pl_2014,
    pgen_2014 = pgen_2014,
    ppathl_demo = ppathl_demo,
    pl_panel = pl_panel,
    pgen_panel = pgen_panel
  ),
  file.path(output_dir, "prepared_inputs.rds")
)
