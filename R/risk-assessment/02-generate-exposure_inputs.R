# 02_exposure_inputs.R ----------------------------------------------------
# Purpose:
#   Convert exposure_inputs.csv (wide, mixed-use)
#   into a clean long-format parameter definition table
#   suitable for Option B + Monte Carlo simulation
# -----------------------------------------------------------------------

library(tidyverse)
library(readr)

# -----------------------------------------------------------------------
# Read raw exposure inputs
# -----------------------------------------------------------------------

raw_inputs <- read_csv(
  "data/processed/exposure_inputs.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------
# Basic cleaning / renaming
# -----------------------------------------------------------------------

raw_inputs <- raw_inputs %>%
  rename(
    age_group = group
  ) %>%
  mutate(
    min_age_yr = as.numeric(min_age_yr),
    max_age_yr = as.numeric(max_age_yr)
  ) %>%
  drop_na(geometric_mean_bll_ug_dl:half_life)

# -----------------------------------------------------------------------
# 1. Baseline BLL (from CHMS percentiles)
# -----------------------------------------------------------------------

baseline_bll <- raw_inputs %>%
  select(
    age_group, min_age_yr, max_age_yr,
    p10_BLL_ug_dl, p50_BLL_ug_dl, p90_BLL_ug_dl
  ) %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "quantile",
    values_to = "value"
  ) %>%
  drop_na(age_group) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value
  ) %>%
  transmute(
    age_group, min_age_yr, max_age_yr,
    parameter = "baseline_bll_ug_dl",
    units = "ug/dL",
    dist = "lognormal",
    p05 = p10_BLL_ug_dl,
    p50 = p50_BLL_ug_dl,
    p95 = p90_BLL_ug_dl,
    source = "CHMS",
    notes = "Baseline blood lead distribution"
  ) %>%
  drop_na(p50)

# -----------------------------------------------------------------------
# 2. Other exposure & biokinetic parameters
#    (treated as fixed or weakly uncertain for now)
# -----------------------------------------------------------------------

other_params <- raw_inputs %>%
  select(
    age_group, min_age_yr, max_age_yr,
    mean_intake_mL_day,
    absorption_fraction_w,
    bsf_ug_dl_ug_day,
    half_life
  ) %>%
  pivot_longer(
    cols = c(
      mean_intake_mL_day,
      absorption_fraction_w,
      bsf_ug_dl_ug_day,
      half_life
    ),
    names_to = "parameter",
    values_to = "p50"
  ) %>%
  mutate(
    units = case_when(
      parameter == "mean_intake_mL_day"       ~ "mL/day",
      parameter == "absorption_fraction_w"    ~ "unitless",
      parameter == "bsf_ug_dl_ug_day"          ~ "ug/dL per ug/day",
      parameter == "half_life"                 ~ "days",
      TRUE ~ NA_character_
    ),
    dist = "fixed",
    p05 = NA_real_,
    p95 = NA_real_,
    source = case_when(
      parameter == "mean_intake_mL_day"    ~ "US EPA EFH",
      parameter == "absorption_fraction_w" ~ "Bowers (1994)",
      parameter == "bsf_ug_dl_ug_day"       ~ "DeSimone et al. (2020)",
      parameter == "half_life"              ~ "ATSDR/EPA",
      TRUE ~ NA_character_
    ),
    notes = NA_character_
  ) %>%
  drop_na(p50)

# -----------------------------------------------------------------------
# 3. Combine into unified parameter-definition table
# -----------------------------------------------------------------------

exposure_param_defs <- bind_rows(
  baseline_bll,
  other_params
) %>%
  arrange(age_group, parameter)

# -----------------------------------------------------------------------
# 4. Scenario grid (kept separate from uncertainty)
# -----------------------------------------------------------------------

scenario_grid <- tibble(
 # fraction_from_system = c(0,0.5,1.0) # pick % you want to test
  fraction_from_system = c(0, 0.25, 0.5, 0.75, 1.0)
)

# -----------------------------------------------------------------------
# 5. Final parameter table (definitions × scenarios)
# -----------------------------------------------------------------------

exposure_parameter_table <- exposure_param_defs %>%
  crossing(scenario_grid)

# -----------------------------------------------------------------------
# Write output
# -----------------------------------------------------------------------

#dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# write_rds(
#   exposure_parameter_table,
#   "data/processed/exposure_parameter_defs-NEW.rds"
# )
