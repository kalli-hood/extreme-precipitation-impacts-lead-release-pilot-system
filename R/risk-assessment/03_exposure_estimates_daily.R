# 03_exposure_estimates_daily.R --------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

pb_daily <- read_rds("data/processed/pb_pred_daily_HRA.rds")
#exposure_factors <- read_rds("data/processed/exposure_factors2.rds")
exposure_inputs <- read_csv("data/processed/exposure_inputs.csv") |>
  mutate(across(min_age_yr:bsf_ug_dl_ug_day, ~ as.numeric(.x)),
         analysis_group = ifelse(is.na(geometric_mean_bll_ug_dl), 0, 1)) |>
  filter(analysis_group == 1)


# data manipulation -------------------------------------------------------

pb_daily <- pb_daily |>
  rename(Pb_ugL_p50 = Pb_ugL_median) |>
  pivot_longer(cols = Pb_ugL_p50:Pb_ugL_p25, 
               names_to = "percentile", values_to = "value",
               names_prefix = "Pb_ugL_")

# ---- Build daily exposure table ------------------------------------------
# dose = effective drinking-water Pb concentration (µg/L) experienced by that age group
# based on their intake levels, median concentration of that day, and fraction 
# of total water intake they consume from system

## Formulation:
# Intake_Dose_Pb_w(t) = [Pb]_w,t * intake_rate * fraction_from_system

pb_daily_exposure <- pb_daily %>%
  mutate(date = as.Date(date)) %>%
  rename(period_flood = period) %>%
  crossing(
     frac_from_system = c(0.25, 0.5, 0.75, 1),
     exposure_inputs
      ) %>%
  mutate(
    intake_L_day = mean_intake_mL_day /1000,
    intake_ug_day = value * intake_L_day * frac_from_system,
    absorbed_ug_day_w = intake_ug_day * absorption_fraction_w
  )

# write_rds(pb_daily_exposure, "data/processed/exposure_estimates.rds")
# 

