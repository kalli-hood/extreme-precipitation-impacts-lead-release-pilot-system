## This file creates the compiled GAMM 
## using TOC, Al, and Fe as input vars for 

# libraries ---------------------------------------------------------------

library(data.table)

# import ------------------------------------------------------------------

# import the datasets for modelling
files <- list.files(path = "data/model-in/", pattern = ".rds", full.names = TRUE)
model_list <- map(files, readRDS)
names(model_list) <- gsub(pattern = "\\.rds$", replacement = "", x = basename(files))
list2env(model_list, envir = .GlobalEnv)
rm(list = setdiff(ls(), c("model_in_pb", "model_in_fe",
                          "model_in_al", "model_in_org_oa_f")))


flood_date <- as.Date("2023-07-24")
flood_date_num <- as.numeric(flood_date)

# setup -------------------------------------------------------------------

source("R/figures/figure-helpers.R")


# m_toc_f <- readRDS("outputs/models_NEW/m_toc_f.rds")
# m_toc <- readRDS("outputs/models_NEW/m_toc.rds")

# Set up data for modelling -----------------------------------------------

# subset of Organic Data --  Finished only
toc_oa_sub <- model_in_org_oa_f %>%
  filter(location == "Finished") |>
  group_by(year, week) %>%
  summarize(
   # date_numeric = mean(date_numeric),
    toc_oa = mean(toc),
    suva = mean(suva),
    uv254 = mean(uv254)
  )

# Extract Aluminum and Iron data and put back into Pb dataset
al_dat <- model_in_al %>%
  select(date, series, value, log_value) %>%
  rename(
    al_ppb = value,
    log_al = log_value
  )

fe_dat <- model_in_fe %>%
  select(date, series, value, log_value) %>%
  rename(
    fe_ppb = value,
    log_fe = log_value
  )

## Adding Al & Fe to Pb model dataset
model_in_pb <- model_in_pb %>%
  rename(
    pb_ppb = value,
    log_pb = log_value
  ) %>%
  left_join(al_dat, by = c("date", "series")) %>%
  left_join(fe_dat, by = c("date", "series")) %>%
  mutate(log_pb = log(pb_ppb),
         main = factor(main, levels = c("PVC", "CI")))


model_in <- model_in_pb %>%
  group_by(date, date_numeric, pipe, main, series) |>
  summarize(
    pb_ppb = median(pb_ppb, na.rm = TRUE),
    log_pb = median(log_pb, na.rm = TRUE),
    al_ppb = median(al_ppb, na.rm = TRUE),
    log_al = median(log_al, na.rm = TRUE),
    fe_ppb = median(fe_ppb, na.rm = TRUE),
    log_fe = median(log_fe, na.rm = TRUE),
  ) |>
  ungroup() |>
  transmute(
    date = as.Date(date_numeric),
    days = date_numeric - flood_date_num,
    date_numeric,
    year = lubridate::year(date),
    week = lubridate::isoweek(date),
    # year, week,
    yday = lubridate::yday(date),
    #  days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days > 0, 1, 0),
    replicate = as.numeric(str_sub(series, -1)),
    pipe, main, series,
    pb_ppb, al_ppb, fe_ppb,
    log_pb, log_al, log_fe
  ) |>
  left_join(toc_oa_sub, by = c("year", "week")) 

# Prepare Data for Modelling ----------------------------------------------
newdat <- model_in %>%
  mutate(flood = factor(ifelse(post_flood == 0, -1, 1)),
         days = date_numeric - flood_date_num,
         main = factor(main, levels = c("PVC", "CI")),
         pipe = factor(pipe, levels = c("pb", "cu-pb"))
        ) %>%
  drop_na(toc_oa, uv254)

# UV and TOC observed in pipe rack may lag behind measured values
## create this lag to eval later
newdat <- newdat %>%
  mutate(
    toc7 = lag(toc_oa, 7),
    toc3 = lag(toc_oa, 3),
    toc5 = lag(toc_oa, 5),
    uv3 = lag(uv254, 3),
    uv5 = lag(uv254, 5),
    uv7 = lag(uv254, 7)
  )

rm(list = c("al_dat", "fe_dat", "model_in_al", "model_in_fe",
            "toc_oa", "toc_oa_sub"))


# Baseline Pb model -------------------------------------------------------
newdat2 <- newdat %>% drop_na(toc_oa, log_fe, log_al)

m_pb1 <- gamm(
  log(pb_ppb) ~
    main + pipe + post_flood +
   s(date_numeric) +
  s(series, bs = "re"),
   correlation = corCAR1(form = ~ date_numeric | series),
  data = newdat2,
  method = "REML"
)
summary(m_pb1$gam)
draw(m_pb1$gam)

# Base + Fe + Al + TOC ----------------------------------------------------

m_pb_mech <- gamm(
  log(pb_ppb) ~
    main + pipe + post_flood +
    s(toc_oa, k = 4) +
    s(log_fe, k = 4) +
    s(log_al, k = 4) +
    s(date_numeric, k =6) +
    s(series, bs = "re"),
  correlation = corCAR1(form = ~ date_numeric | series),
  data = newdat,
  method = "REML"
)
summary(m_pb_mech$gam)
draw(m_pb_mech$gam)
gam.check(m_pb_mech$gam)

AIC(m_pb_mech$lme, m_pb1$lme)



# extract model estimates -------------------------------------------------

model_estimates_mech <- broom::tidy(m_pb_mech$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ exp(.x)))

# write -------------------------------------------------------------------
# 
# write_csv(model_estimates_mech, file = "outputs/model-coefficients/m_pb_mech_estimates.csv")
