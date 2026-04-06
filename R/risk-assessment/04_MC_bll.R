# -------------------------------------------------------------------------
# 04_monte_carlo_bll_optionB_chunked.R
#
# Purpose:
#   Memory-safe Monte Carlo simulation of dynamic BLL (Option B)
#   using numeric age bins (min_age_yr, max_age_yr), not age_group labels.
#
# Model:
#   BLL_water(t) = r * BLL_water(t-1) + BSF * absorbed_water_intake(t)
#   BLL_total(t) = baseline_bll + BLL_water(t)
#
# Design:
#   - Chunk by (min_age_yr, max_age_yr, fraction_from_system)
#   - Sample parameters per block
#   - Loop over time (days), vectorized over sims
#   - Store only summaries (mean, quantiles, exceedance probability)
# -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

n_sims <- 2000
threshold_ug_dl <- 3.5
set.seed(123)

# -------------------------------------------------------------------------
# Helper functions: distribution fitting & sampling
# -------------------------------------------------------------------------

# This function estimates lof mu and sigma from the
## quantile values
fit_lognormal_from_quantiles <- function(p05, p50, p95) {
  z05 <- qnorm(0.05)
  z95 <- qnorm(0.95)
  list(
    meanlog = log(p50),
    sdlog   = (log(p95) - log(p05)) / (z95 - z05)
  )
}

sample_param <- function(dist, p05, p50, p95, n, name) {
  
  dist <- tolower(dist)
  
  if (dist == "fixed") {
    if (is.na(p50)) stop("Fixed parameter missing p50: ", name)
    return(rep(p50, n))
  }
  
  if (dist == "lognormal") {
    if (any(is.na(c(p05, p50, p95))) || any(c(p05, p50, p95) <= 0)) {
      stop("Invalid lognormal quantiles for ", name)
    }
    pars <- fit_lognormal_from_quantiles(p05, p50, p95)
    return(rlnorm(n, pars$meanlog, pars$sdlog))
  }
  stop("Unknown distribution for ", name)
}

# -------------------------------------------------------------------------
# Read inputs
# -------------------------------------------------------------------------

param_defs <- readRDS("data/processed/exposure_parameter_defs-NEW.rds")

pb_daily <- readRDS("data/processed/pb_pred_daily_HRA.rds") %>%
  rename(Pb_ugL_p50 = Pb_ugL_median) |>
  pivot_longer(cols = Pb_ugL_p50:Pb_ugL_p25, 
               names_to = "percentile", values_to = "Pb_ugL",
               names_prefix = "Pb_ugL_") |> 
  mutate(date = as.Date(date)) %>%
  select(date, period, percentile, Pb_ugL)

# -------------------------------------------------------------------------
# Required parameters
# -------------------------------------------------------------------------

required_params <- c(
  "mean_intake_mL_day",
  "absorption_fraction_w",
  "baseline_bll_ug_dl",
  "bsf_ug_dl_ug_day",
  "half_life"
)

# -------------------------------------------------------------------------
# Define blocks by numeric age bins + fraction
# -------------------------------------------------------------------------

## these are the blocks from which we make estimates.
block_keys <- param_defs %>%
  distinct(min_age_yr, max_age_yr, fraction_from_system) %>%
  arrange(min_age_yr, max_age_yr, fraction_from_system)

# -------------------------------------------------------------------------
# Sample parameters for one block
# -------------------------------------------------------------------------

# This function is essentially a wrapper for sampke_params()
## with some safety nets. It creates the inputs required (with uncertainty)
## to feed into the BLL model.

sample_block_params <- function(def_block, n_sims) {
  
  defs <- def_block %>%
    filter(parameter %in% required_params) %>%
    select(parameter, dist, p05, p50, p95)
  
  missing <- setdiff(required_params, defs$parameter)
  if (length(missing) > 0) {
    stop(
      "Block missing parameters: ",
      paste(missing, collapse = ", "),
      "\nAge range: ", unique(def_block$min_age_yr), "-",
      unique(def_block$max_age_yr),
      "\nFraction: ", unique(def_block$fraction_from_system)
    )
  }
  
  draws <- list()
  for (p in required_params) {
    row <- defs %>% filter(parameter == p)
    draws[[p]] <- sample_param(
      dist = row$dist,
      p05  = row$p05,
      p50  = row$p50,
      p95  = row$p95,
      n    = n_sims,
      name = p
    )
  }
  
  draws
}

# -------------------------------------------------------------------------
# Run one block (vectorized over sims, loop over days)
# -------------------------------------------------------------------------


run_block_optionB <- function(pb_daily, draws, keys, threshold) {
  
  # From "draws" create unique simulated values for the exposure/physiological 
  ## parameters for each block.
  intake_L <- draws$mean_intake_mL_day / 1000
  AF       <- draws$absorption_fraction_w
  baseline <- draws$baseline_bll_ug_dl
  bsf      <- draws$bsf_ug_dl_ug_day
  k        <- log(2) / draws$half_life
  r        <- exp(-k)
  
  frac_sys <- keys$fraction_from_system
  
  # split once by Pb quantile & run the model separately on each
  pb_blocks <- split(pb_daily, pb_daily$percentile)
  
  out_all <- vector("list", length(pb_blocks))
  names(out_all) <- names(pb_blocks)
  
  # inside each Pb quantile block:
  for (q in names(pb_blocks)) {
    
    pb_q <- pb_blocks[[q]]
    # incremental BLL due to water set to 0 to start
    bll_water <- rep(0, length(intake_L)) 
    out_q <- vector("list", nrow(pb_q))
    
    # for each DAY
    for (i in seq_len(nrow(pb_q))) {
      
      Pb <- pb_q$Pb_ugL[i]
      
      # compute daily Pb absorbed
      absorbed  <- Pb * intake_L * frac_sys * AF
      # incremental BLL considers the retained value from the prior day(s)
      bll_water <- r * bll_water + absorbed * bsf
      # compute total bll for that day by adding incremental to baseline.
      bll_total <- baseline + bll_water
      
      out_q[[i]] <- tibble(
        date = pb_q$date[i],
        period = pb_q$period[i],
        pb_quantile = q,
        min_age_yr = keys$min_age_yr,
        max_age_yr = keys$max_age_yr,
        fraction_from_system = frac_sys,
        bll_mean = mean(bll_total),
        bll_p05  = quantile(bll_total, 0.05),
        bll_p50  = quantile(bll_total, 0.50),
        bll_p95  = quantile(bll_total, 0.95),
        p_exceed = mean(bll_total > threshold),
        n_sims   = length(bll_total)
      )
    }
    
    out_all[[q]] <- bind_rows(out_q)
  }
  
  bind_rows(out_all)
}


# -------------------------------------------------------------------------
# Run all blocks
# -------------------------------------------------------------------------

daily_results <- vector("list", nrow(block_keys))

for (b in seq_len(nrow(block_keys))) {
  
  keys <- block_keys[b, ]
  
  message(
    sprintf(
      "[%d/%d] Age %g–%g | frac = %.2f",
      b, nrow(block_keys),
      keys$min_age_yr, keys$max_age_yr,
      keys$fraction_from_system
    )
  )
  
  def_block <- param_defs %>%
    filter(
      min_age_yr == keys$min_age_yr,
      max_age_yr == keys$max_age_yr,
      fraction_from_system == keys$fraction_from_system
    )
  
  draws <- sample_block_params(def_block, n_sims)
  
 # str(draws) ## check -- need to remove
  
  daily_results[[b]] <- run_block_optionB(
    pb_daily = pb_daily,
    draws = draws,
    keys = keys,
    threshold = threshold_ug_dl
  )
  
  rm(draws)
  gc()
}

daily_summary <- bind_rows(daily_results)

write_rds(daily_summary, "data/processed/bll_daily_summary.rds")
#write_csv(daily_summary, "data/processed/bll_optionB_daily_summary.csv")

# -------------------------------------------------------------------------
# Period summaries
# -------------------------------------------------------------------------

period_summary <- daily_summary %>%
  group_by(period, min_age_yr, max_age_yr, pb_quantile, fraction_from_system) %>%
  summarise(
    p_exceed_mean = mean(p_exceed),
    bll_mean_mean = mean(bll_mean),
    bll_p95_mean  = mean(bll_p95),
    n_days = n(),
    n_sims = max(n_sims),
    .groups = "drop"
  )

# write_rds(period_summary, "data/processed/bll_period_summary.rds")
# #write_csv(period_summary, "data/processed/bll_optionB_period_summary.csv")
# 


