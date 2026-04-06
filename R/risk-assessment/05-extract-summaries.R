daily_summary <- readRDS("data/processed/bll_daily_summary.rds") %>%
  filter(pb_quantile %in% c("p05", "p25", "p50", "p75", "p95")) %>%
  mutate(
    scenario = case_when(
      pb_quantile == "p05" ~ "low",    
      pb_quantile == "p50" ~ "mid",
      pb_quantile == "p95" ~ "high"
    )
  ) 

daily_summary_wide <- daily_summary %>%
  drop_na(scenario) %>%
  select(date, period, min_age_yr, fraction_from_system:scenario) %>%
 # filter(scenario == "high") %>%
  pivot_wider(
    names_from = scenario,
    values_from = bll_mean:p_exceed
  ) 

pb_daily <- readRDS("data/processed/pb_pred_daily_HRA.rds")

# # Optional
results_period <- readRDS("data/processed/bll_period_summary.rds")

# Extract peak BLL & date  ------------------------------------------------
peak_bll <- daily_summary_wide %>%
  filter(
   # pb_quantile == "p75",
    fraction_from_system == 0.5,
    period == "Disturbance",
   # min_age_yr > 17
  #  min_age_yr %in% c(3, 4, 5, 15, 30, 60)
  ) %>%
  group_by(min_age_yr) %>%
  summarize(
 #   peak_bll = max(bll_p50_mid, na.rm = TRUE),
  #  peak_bll_p05 = max(bll_p05_mid, na.rm = TRUE),
  #  peak_bll_p95 = max(bll_p95_mid, na.rm = TRUE),
  #  peak_date = date[which.max(bll_p50_mid)]
   #  peak_bll = max(bll_p50_high, na.rm = TRUE),
   # peak_bll_p05 = max(bll_p05_high, na.rm = TRUE),
   # peak_bll_p95 = max(bll_p95_high, na.rm = TRUE),
   # peak_date = date[which.max(bll_p50_high)]
    peak_bll = max(bll_p50_low, na.rm = TRUE),
    peak_bll_p05 = max(bll_p05_low, na.rm = TRUE),
    peak_bll_p95 = max(bll_p95_low, na.rm = TRUE),
    peak_date = date[which.max(bll_p50_low)]
  
  ) %>%
  ungroup()

peak_bll



# extract peak Pb level & date --------------------------------------------

pb_peak_date <- pb_daily %>%
  filter(period == "Disturbance") %>%
  summarise(
    pb_peak_p05 = max(Pb_ugL_p05),
    pb_peak_p50 = max(Pb_ugL_median),
    pb_peak_p95 = max(Pb_ugL_p95),
    pb_peak_p05_date = date[which.max(Pb_ugL_p05)],
    pb_peak_p50_date = date[which.max(Pb_ugL_median)],
    pb_peak_p95_date = date[which.max(Pb_ugL_p95)]
    
  )



# comput lag --------------------------------------------------------------

lag_bll <- peak_bll %>%
  mutate(
    lag_days = as.numeric(peak_date - pb_peak_date$pb_peak_p50_date)
  )

lag_bll



# mean exceedance probability by period -----------------------------------

period_exceed <- daily_summary_wide %>%
  filter(
    date < as.Date("2024-07-01"),
    pb_quantile == "p50",
    fraction_from_system == 0.5,
    min_age_yr %in% c(5, 15, 30, 60),
  ) %>%
  group_by(min_age_yr, period) %>%
  summarise(
    mean_p_exceed = mean(p_exceed_mid),
    sd_p_exceed   = sd(p_exceed_mid),
    .groups = "drop"
  )

period_exceed



# sensitivity range across scenarios --------------------------------------

scenario_range <- daily_summary_wide %>%
  filter(period == "Disturbance",
         min_age_yr %in% c(5, 15, 30, 60)) %>%
  group_by(min_age_yr) %>%
  summarise(
    min_exceed = min(p_exceed_mid),
    max_exceed = max(p_exceed_mid),
    .groups = "drop"
  )

scenario_range

# recovery time -----------------------------------------------------------

baseline_bll <- daily_summary_wide %>%
  filter(
    pb_quantile == "p50",
    fraction_from_system == 0.5,
    period == "Pre-flood"
  ) %>%
  group_by(min_age_yr) %>%
  summarise(
    baseline_bll = mean(bll_p50_mid),
    .groups = "drop"
  )

recovery_time <- daily_summary_wide %>%
  filter(
    pb_quantile == "p50",
    fraction_from_system == 0.5,
     period != "Pre-flood",
    date > as.Date("2023-08-01"),
    min_age_yr %in% c(3, 4, 5, 15, 30, 60)
  ) %>%
  left_join(baseline_bll, by = "min_age_yr") %>%
  group_by(min_age_yr) %>%
  filter(abs(bll_p50_mid - baseline_bll) / baseline_bll <= 0.10) %>%
  summarise(
    recovery_date = min(date),
    .groups = "drop"
  )

