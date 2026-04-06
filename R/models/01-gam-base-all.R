# Generalized additive model for 
## pipe loop metals, UV, TOC from sections 1.1 and 1.2 
## 1.2.1 is in a different file

# libraries ---------------------------------------------------------------

library(broom)
library(tidyverse)
library(janitor)
library(mgcv)
library(gratia)
library(data.table)

# import ------------------------------------------------------------------

# import the datasets for modelling
files <- list.files(path = "data/model-in/", pattern = ".rds", full.names = TRUE)
model_list <- map(files, readRDS)
names(model_list) <- gsub(pattern = "\\.rds$", replacement = "", x = basename(files))


# SI Section 1.1 -----------------------------------------------------

# UV - Raw water
m_uv_raw <- mgcv::gamm(
  log(uv) ~
     post_flood +
    s(date_numeric) +
    s(yday, bs = "cc"),
  data = model_list$model_in_uvr,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
#summary(m_uv_raw$gam)
#appraise(m_uv_raw$gam)
#gam.check(m_uv_raw$gam)
#draw(m_uv_raw$gam)

# UV - Finished 
m_uv_fin <- mgcv::gamm(
  log(uv) ~ 
    post_flood +
    s(date_numeric) +
    s(yday, bs = "cc"),
  data = model_list$model_in_uvf,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_uv_fin$gam)
# appraise(m_uv_fin$gam)
# gam.check(m_uv_fin$gam)
# draw(m_uv_fin$gam)


# TOC 
m_toc <- mgcv::gamm(
  log(toc) ~
    post_flood +
    s(date_numeric) +
    s(yday, bs = "cc"),
  data = model_list$model_in_toc_grab_f,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_toc$gam)
# appraise(m_toc$gam)
# gam.check(m_toc$gam)
# draw(m_toc$gam)

# TOC online analyzer ---------------------------------------------------------------------
## Raw

model_in_org_oa_r <- model_list$model_in_org_oa_r %>%
  filter(days < 100)

m_toc_r <- mgcv::gamm(
  log(toc) ~
     post_flood +
    s(days, k=6),
  data = model_in_org_oa_r,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_toc_r$gam)
# appraise(m_toc_r$gam)
# gam.check(m_toc_r$gam)
# draw(m_toc_r$gam)


## UV FROM OA
m_uv_r <- mgcv::gamm(
  log(uv254) ~
    post_flood +
    s(days, k= 6),
  data = model_in_org_oa_r,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_uv_r$gam)
# appraise(m_uv_r$gam)
# gam.check(m_uv_r$gam)
# draw(m_uv_r$gam)


## FINISHED-- OA

# TOC
model_in_toc_oa_f <- model_list$model_in_org_oa_f %>%
  filter(days < 100)

m_toc_f <- mgcv::gamm(
  log(toc) ~
    post_flood +
    s(days, k= 6),
  data = model_in_toc_oa_f,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_toc_f$gam)
# appraise(m_toc_f$gam)
# gam.check(m_toc_f$gam)
# draw(m_toc_f$gam)

# tidy(m_toc_f$gam, parametric = TRUE, conf.int = TRUE) %>%
#   mutate(across(c(estimate, conf.low, conf.high), ~ exp(.x)))

# TOC
m_uv_f <- mgcv::gamm(
  log(uv254) ~
    post_flood +
    s(days, k= 6),
  data = model_list$model_in_org_oa_f,
  method = "REML"
)

# ---- DIAGNOSTICS ---------#
# summary(m_uv_f$gam)
# appraise(m_uv_f$gam)
# gam.check(m_uv_f$gam)
# draw(m_uv_f$gam)



# SI Section 1.2 ---------------------------------------------------------
# Iron --------------------------------------------------------------------

m_fe <- mgcv::gamm(
  log(value) ~ 
    dist_main + lsl + 
    post_flood +
    s(date_numeric) +
    s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_fe,
  method = "REML"
)

# summary(m_fe$gam)
# appraise(m_fe$gam)
# gam.check(m_fe$gam)
# draw(m_fe$gam)


# Copper ------------------------------------------------------------------

m_cu <- mgcv::gamm(
  log_value ~ 
    dist_main + lsl + post_flood +
    s(date_numeric) +
    s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_cu,
  method = "REML"
)

# summary(m_cu$gam)
# appraise(m_cu$gam)
# gam.check(m_cu$gam)
# draw(m_cu$gam)


# Lead --------------------------------------------------------------------

# model_in_pb <- model_in_pb %>%
#   mutate(flood = factor(ifelse(post_flood == 0, -1, 1)),
#          days = date_numeric - flood_date_num,
#          days_rel = ifelse(post_flood == 1, days_since_flood, NA))

m_pb <- mgcv::gamm(
  log_value ~
    dist_main + lsl + post_flood +
    s(date_numeric) +
    s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
    #s(days_since_flood, k=12),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_pb,
  method = "REML"
)

# summary(m_pb$gam)
# appraise(m_pb$gam)
# gam.check(m_pb$gam)
# draw(m_pb$gam)



# Pb subset -- duration model ---------------------------------------------
model_in_pb <- model_list$model_in_pb %>%
  mutate(
    date = 
  )

pb_window <- subset(model_list$model_in_pb, days > -150  & days < 150)
pb_window2 <- subset(model_list$model_in_pb, days > -150 & days < 360)

m_pb_duration <- mgcv::gamm(
  log_value ~ dist_main + lsl +
    s(series, bs = "re") +
    s(days, k = 10),
  data = pb_window,
  method = "REML"
)

# summary(m_pb_duration$gam)
# appraise(m_pb_duration$gam)
# draw(m_pb_duration$gam)

m_pb_duration2 <- mgcv::gamm(
  log_value ~ dist_main + lsl + post_flood +
    s(series, bs = "re") +
    s(days),
  data = pb_window2,
  method = "REML"
)
# 
# summary(m_pb_duration2$gam)
# appraise(m_pb_duration2$gam)
# draw(m_pb_duration2$gam)


# Manganese ---------------------------------------------------------------

m_mn <- mgcv::gamm(
  log_value ~ 
    dist_main + #lsl + 
    post_flood +
    s(date_numeric) + 
    s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_mn,
  method = "REML"
)

# summary(m_mn$gam)
# appraise(m_mn$gam)
# gam.check(m_mn$gam)
# draw(m_mn$gam)

# Aluminum ----------------------------------------------------------------

m_al <- mgcv::gamm(
  log_value ~ 
    dist_main + #lsl + 
    post_flood +
    s(date_numeric) +
  #  s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_al,
  method = "REML"
)

# summary(m_al$gam)
# appraise(m_al$gam)
# gam.check(m_al$gam)
# draw(m_al$gam)


# Phosphate ---------------------------------------------------------------

m_p <- mgcv::gamm(
  log(value) ~ 
    dist_main + lsl + post_flood +
    s(date_numeric) +
   # s(date_numeric, series, m = 1, bs = "fs") +
    s(yday, bs = "cc") +
    s(series, bs = "re"),
  correlation = nlme::corCAR1(form = ~ date_numeric | series),
  data = model_list$model_in_p
)

# summary(m_p$gam)
# appraise(m_p$gam)
# gam.check(m_p$gam)
# draw(m_p$gam)


# tidy model outputs ------------------------------------------------------

model_smooths_uvr <- tidy(m_uv_raw$gam, parametric = FALSE) %>%
  mutate(model = "UV Raw")
model_estimates_uvr <- tidy(m_uv_raw$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "UV Raw",
    across(c(estimate, conf.low, conf.high))
  )

model_smooths_uvf <- tidy(m_uv_fin$gam, parametric = FALSE) %>%
  mutate(model = "UV Finished")
model_estimates_uvf <- tidy(m_uv_fin$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "UV Finished",
    across(c(estimate, conf.low, conf.high))
  )

model_smooths_toc <- tidy(m_toc$gam, parametric = FALSE) %>%
  mutate(model = "TOC")
model_estimates_toc <- tidy(m_toc$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total TOC",
    across(c(estimate, conf.low, conf.high))
  )

model_smooths_toc_r <- tidy(m_toc_r$gam, parametric = FALSE) %>%
  mutate(model = "TOC Raw")
model_estimates_toc <- tidy(m_toc_r$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "TOC Raw",
    across(c(estimate, conf.low, conf.high))
  )

model_smooths_toc_f <- tidy(m_toc_f$gam, parametric = FALSE) %>%
  mutate(model = "TOC Finished")
model_estimates_toc <- tidy(m_toc_f$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "TOC Finished",
    across(c(estimate, conf.low, conf.high))
  )

model_smooths_fe <- tidy(m_fe$gam, parametric = FALSE) %>%
  mutate(model = "Total Fe")
model_estimates_fe <- tidy(m_fe$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total Fe",
    across(c(estimate, conf.low, conf.high), ~ exp(.x))
  )

model_smooths_cu <- tidy(m_cu$gam, parametric = FALSE) %>%
  mutate(model = "Total Cu")
model_estimates_cu <- tidy(m_cu$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total Cu",
    across(c(estimate, conf.low, conf.high), ~ exp(.x))
  )

model_smooths_pb <- tidy(m_pb$gam, parametric = FALSE) %>%
  mutate(model = "Total Pb")
model_estimates_pb <- tidy(m_pb$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total Pb",
    across(c(estimate, conf.low, conf.high), ~ exp(2*.x))
  )

model_smooths_mn <- tidy(m_mn$gam, parametric = FALSE) %>%
  mutate(model = "Total Mn")
model_estimates_mn <- tidy(m_mn$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total Mn",
    across(c(estimate, conf.low, conf.high), ~ exp(.x))
  )

model_smooths_al <- tidy(m_al$gam, parametric = FALSE) %>%
  mutate(model = "Total Al")
model_estimates_al <- tidy(m_al$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total Al",
    across(c(estimate, conf.low, conf.high), ~ exp(.x))
  )

model_smooths_p <- tidy(m_p$gam, parametric = FALSE) %>%
  mutate(model = "Total P")
model_estimates_p <- tidy(m_p$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(
    model = "Total P",
    across(c(estimate, conf.low, conf.high), ~ exp(.x))
  )



# combine model outputs ---------------------------------------------------

model_estimates_total_metals <- rbind(model_estimates_al, model_estimates_cu,
                         model_estimates_fe, model_estimates_mn, 
                         model_estimates_p, model_estimates_pb)
                         # model_estimates_toc, model_estimates_uvf, 
                         # model_estimates_uvr)

model_smooths_total_metals <- rbind(model_smooths_al, model_smooths_cu,
                         model_smooths_fe, model_smooths_mn, 
                         model_smooths_p, model_smooths_pb)
                         # model_smooths_toc, model_smooths_uvf, 
                         # model_smooths_uvr)

# save models -------------------------------------------------------------
# 
# saveRDS(m_al, file = "outputs/models/m_al.rds")
# saveRDS(m_cu, file = "outputs/models/m_cu.rds")
# saveRDS(m_fe, file = "outputs/models/m_fe.rds")
# saveRDS(m_mn, file = "outputs/models/m_mn.rds")
# saveRDS(m_p, file = "outputs/models/m_p.rds")
# saveRDS(m_pb, file = "outputs/models/m_pb.rds")
# saveRDS(m_toc, file = "outputs/models/m_toc.rds")
# saveRDS(m_toc_r, file = "outputs/models/m_toc_r.rds")
# saveRDS(m_toc_f, file = "outputs/models/m_toc_f.rds")
# saveRDS(m_uv_r, file = "outputs/models/m_uv_r.rds")
# saveRDS(m_uv_f, file = "outputs/models/m_uv_f.rds")
# saveRDS(m_uv_fin, file = "outputs/models/m_uv_fin.rds")
# saveRDS(m_uv_raw, file = "outputs/models/m_uv_raw.rds")
# saveRDS(m_pb_duration, file = "outputs/models/m_pb_duration.rds")
# 
# 
# write_csv(model_estimates_total_metals, file = "outputs/model-coefficients/model_estimates_tot_metals.csv")
# write_csv(model_smooths_total_metals, file = "outputs/model-coefficients/model_smooths_tot_metals.csv")
