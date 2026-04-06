## this makes what is figure 2 in v4 of the manuscript draft
## time series of organics, pb , temp and precip


# libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)
library(mgcv)
library(gratia)
library(stringr)
library(data.table)
library(wesanderson)
library(PNWColors)
library(lubridate)

pal <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- pal[c(1,6)]
pal6 <- pal[c(2,3,4,5,6,1)]
alpha <- .3



# import ------------------------------------------------------------------
source("R/models/01-gam-setup-NEW.R")
source("R/figures/figure-helpers.R")

# model_in_pb <- readRDS("outputs/models_NEW/m_pb.rds")
m_pb <- readRDS("outputs/models_NEW/m_pb.rds")
m_toc <- readRDS("outputs/models_NEW/m_toc.rds")
m_uv_fin <- readRDS("outputs/models_NEW/m_uv_fin.rds")
m_uv_raw <- readRDS("outputs/models_NEW/m_uv_raw.rds")

# setup -------------------------------------------------------------------

pal <- wes_palette("Zissou1", type = "discrete")

# get flood_date
flood_date_numeric = as.numeric(as.Date("2023-07-24"))


# build pred grid ---------------------------------------------------------
# create a new DF to generate predictions

newdat_toc <- toc |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days_since_flood = date_numeric - flood_date_numeric,
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days_since_flood > 0, 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date)
  ) |>
  fitted_values(m_toc$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), ~ exp(.x)))


newdat_uv <-uv |>
  group_by(location) |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days_since_flood = date_numeric - flood_date_numeric,
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days_since_flood > 0, 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date)
  ) 


newdat_uv_r <- newdat_uv %>% filter(location == "Raw") |> 
  fitted_values(m_uv_raw$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), ~ exp(.x)))
newdat_uv_f <- newdat_uv %>% filter(location == "Finished") |> 
  fitted_values(m_uv_fin$gam, data = _, ci = 0.05) #|>
# mutate(across(c(.fitted, .lower_ci, .upper_ci), ~ exp(.x)))


# create a new DF to generate predictions
newdat_pb <- model_in_pb |>
  group_by(series, dist_main, lsl) |>
  nest() |>
  ungroup() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days_since_flood = date_numeric - flood_date_numeric,
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days_since_flood > 0, 1, 0),
    year = lubridate::year(date),
    replicate = as.numeric(str_sub(series, -1)),
    yday = lubridate::yday(date)
  ) |>
  fitted_values(m_pb$gam, data = _, ci = 0.05) |>     # predict from model using input grid
  mutate(across(c(.fitted, .lower_ci, .upper_ci), exp))  # back-transform prediction

# get smooths -------------------------------------------------------------


global_smooth_pb <- smooth_estimates(m_pb$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Global Smooth", 
         analyte = "Pipe rack, Pb")

global_smooth_tocf <- smooth_estimates(m_toc$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Global Smooth",
         analyte = "Finished TOC")

# seasonal_smooth_tocf <- smooth_estimates(m_toc$gam, select = "s(yday)") %>%
#   compute_sig() %>%
#   arrange(yday) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          smooth = "Seasonal Smooth",
#          analyte = "Finished TOC")

global_smooth_uvr <- smooth_estimates(m_uv_raw$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Global Smooth",
         analyte = "Raw UV254")

# seasonal_smooth_uvr <- smooth_estimates(m_uv_raw$gam, select = "s(yday)") %>%
#   compute_sig() %>%
#   arrange(yday) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          smooth = "Seasonal Smooth",
#          analyte = "Raw UV254")

global_smooth_uvf <- smooth_estimates(m_uv_fin$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Global Smooth",
         analyte = "Finished UV254")

# seasonal_smooth_uvf <- smooth_estimates(m_uv_fin$gam, select = "s(yday)") %>%
#   compute_sig() %>%
#   arrange(yday) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          smooth = "Seasonal Smooth",
#          analyte = "Finished UV254")
# 
# flood_smooth_uvf <- smooth_estimates(m_uv_fin$gam, select = "s(days_since_flood)") %>%
#   compute_sig() %>%
#   arrange(days_since_flood) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          smooth = "Flood Smooth",
#          analyte = "Finished UV254")
# 
# flood_smooth_uvr <- smooth_estimates(m_uv_raw$gam, select = "s(days_since_flood)") %>%
#   compute_sig() %>%
#   arrange(days_since_flood) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          smooth = "Flood Smooth",
#          analyte = "Raw UV254")


#plot --------------------------------------------------------------------

# time series of observed values overlaid with fitted

# Finished TOC + fitted values --------------------------------------------
p1 <- 
  newdat_toc %>%
  mutate(analyte = "TOC (mg/L)",
         location = "Finished") %>%
  ggplot() +
  geom_point(data = toc,
             aes(x = yday, y = toc, col = factor(year)), #group = series, col = factor(lsl)),
             alpha = 0.4) +
  geom_vline(xintercept = 203, linetype = "dashed") +
  # geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")
  geom_line(aes(x = yday, y = .fitted, 
                col = factor(year)),linewidth = 1) +
  facet_wrap(location ~ analyte, strip.position = "right") +
  scale_x_date(date_labels = "%B") +
  scale_colour_manual(values = pal6) +
  labs(col = "", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_blank(),
        text = element_text(size = 12, face = "bold"))
p1


# Raw UV254 + Fitted ------------------------------------------------------


p1b <- 
  newdat_uv_r %>%
  mutate(analyte = "UV254") %>%
  ggplot() +
  geom_point(data = uv %>% filter(location == "Raw"),
             aes(x = yday, y = uv, col = factor(year)), #group = series, col = factor(lsl)),
             size = 0.5, alpha = 0.4) +
  geom_vline(xintercept = 203, linetype = "dashed") +
  # geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")
  geom_line(aes(x = yday, y = .fitted, 
                col = factor(year)), linewidth = 1) +
  facet_wrap(location ~ analyte, strip.position = "right") +
  scale_x_date(date_labels = "%B") +
  scale_colour_manual(values = pal6[3:6]) +
  labs(col = "", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_blank(),
        text = element_text(size = 12, face = "bold"))
p1b


p1c <- newdat_uv_f %>%
  mutate(analyte = "UV254") %>%
  ggplot() +
  geom_point(data = uv %>% filter(location == "Finished"),
             aes(x = yday, y = uv, col = factor(year)), #group = series, col = factor(lsl)),
             size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = 203, linetype = "dashed") +
  # geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")
  geom_line(aes(x = yday, y = .fitted, 
                col = factor(year)), 
           # linewidth = 1
            ) +
  facet_wrap(location ~ analyte, strip.position = "right") +
  scale_x_date(date_labels = "%B") +
  scale_colour_manual(values = pal6[3:6]) +
  labs(col = "", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "top", #axis.text.x = element_blank(),
        text = element_text(size = 12, face = "bold"))

p1c


plotdat <- model_in_pb %>%
  group_by(date, yday, year, lsl, dist_main) %>%
  summarise(p25 = quantile(value, 0.25),
            p50 = quantile(value, 0.5),
            p75 = quantile(value, 0.75)) %>%
  ungroup() %>%
  mutate(lsl = factor(lsl, labels = c("Pb", "Cu-Pb")))

## lead
p2 <- newdat_pb %>%
  group_by(date, yday, year) %>%
  summarise(med_fitted = median(.fitted)) %>%
  ungroup() %>%
  mutate(analyte = "Total Pb (ug/L",
         location = "Pipe Rack") %>%
  ggplot() +
  geom_point(data = plotdat, #%>% filter(location == "Finished"),
             aes(x = yday, y = p50, 
                 col = factor(year), shape = factor(lsl)), #group = series, col = factor(lsl)),
             alpha = 0.2) +
  geom_errorbar(data = plotdat, #%>% filter(location == "Finished"),
             aes(x = yday, ymin = p25, ymax = p75, 
                 col = factor(year), shape = factor(lsl)), #group = series, col = factor(lsl)),
             alpha = 0.2) +
  geom_vline(xintercept = 203, linetype = "dashed") +
  # geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")
  geom_line(aes(x = yday, y = med_fitted, 
                col = factor(year)), linewidth = 1) +
  scale_y_log10() +
 # facet_wrap(~series) +
   facet_wrap(location ~ analyte, strip.position = "right") +
  scale_x_date(date_labels = "%B") +
  scale_colour_manual(values = pal6[5:6]) +
  labs(col = "", y = "", x = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_blank(),
        text = element_text(size = 12, face = "bold"))

p2
source("R/figures/fig-eccdata.R")

c_2

a <- ggarrange(p2, c_2, ncol = 1, heights = c(0.4, 0.6), 
               common.legend = TRUE, align = "v")

b <- ggarrange(p1, p1b, p1c, ncol = 1, common.legend = TRUE)

f3 <- ggarrange(b, a, common.legend = TRUE)

ggsave("outputs/figures/fig3-ts-toc-uv-pb-enviro-dat.png", f3,
       width = 8, height = 6, dpi = 600)
