# 01_pb_preds.R
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(purrr)
library(gratia)

# 1. Load model + data --------------------------------------------
m_pb <-readRDS("outputs/models/m_pb.rds")   # final lead model
pb_raw <- read_rds("data/model-in/model_in_pb.rds")

# 2. Create full date grid per series / pipe ----------------------
pb_grid <- pb_raw %>%
  group_by(series, pipe, main) %>%
  summarise(
    date_min = min(date),
    date_max = max(date),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(date = list(seq(date_min, date_max, by = "day"))) %>%
  unnest(date) %>%
  mutate(
    date_numeric = as.numeric(date),
    yday = yday(date),
    year = year(date),
    dist_main = as.numeric(case_when(
      main == "PVC" ~ -1,
      main == "CI" ~ 1
    )),
    lsl = as.numeric(case_when(
      pipe == "pb" ~ -1,
      pipe == "cu-pb" ~ 1
    )),
    series = factor(series)
  ) 

# 3. Add flood indicator ------------------------------------------
flood_date <- as.Date("2023-07-24")

pb_grid <- pb_grid %>%
  mutate(
    post_flood = if_else(date >= flood_date, 1L, 0L)
  )

# 4. Predict Pb (µg/L) using GAMM --------------------------------
# predict across a continous time range (grid)
pb_pred <- fitted_values(
  m_pb$gam,
  data = pb_grid,
  ci = 0.05
) %>%
 # bind_cols(pb_grid) %>%
  mutate(
    Pb_ugL = exp(.fitted), # back-transform 
    Pb_ugL_lower = exp(.lower_ci),
    Pb_ugL_upper = exp(.upper_ci)
  )

# 5. Aggregate across series (if desired) -------------------------
# Option: median across series to get a system-level daily Pb
pb_daily <- pb_pred %>%
  group_by(date) %>%
  summarise(
    Pb_ugL_median = median(Pb_ugL, na.rm = TRUE),
    Pb_ugL_p95    = quantile(Pb_ugL, 0.95, na.rm = TRUE),
    Pb_ugL_p75    = quantile(Pb_ugL, 0.75, na.rm = TRUE),
    Pb_ugL_p05    = quantile(Pb_ugL, 0.05, na.rm = TRUE),
    Pb_ugL_p25    = quantile(Pb_ugL, 0.25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = case_when(
      date < flood_date ~ "Pre-flood",
      date >= flood_date & date <= flood_date + 80 ~ "Disturbance",
      TRUE ~ "Recovery"
    )
  )

# write_rds(pb_pred, "data/processed/pb_pred_HRA.rds")
# write_rds(pb_daily, "data/processed/pb_pred_daily_HRA.rds")
