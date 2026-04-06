## This is a setup file, intended to be called from one of the 
## scripts making figures

# libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(mgcv)
library(gratia)
library(stringr)
library(data.table)
library(wesanderson)
library(PNWColors)
library(lubridate)

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3


# functions ---------------------------------------------------------------

# --- Helper: classify derivatives into sig = 1/0 ---
compute_sig <- function(df) {
  df %>%
    mutate(
      lower = .estimate - 1.96 * .se,
      upper = .estimate + 1.96 * .se,
      sig   = ifelse(lower > 0, 1,
                     ifelse(upper < 0, -1, 0))
    )
}
# --- Helper: add run IDs to break lines when sig flips ---
add_sig_runs <- function(df) {
  df %>%
    arrange(series, date_numeric) %>%
  #  group_by(series) %>%
    mutate(sig_run = rleid(sig)) %>%
    ungroup()
}

extract_disturbance_metrics <- function(model, smooth_name = "s(days)") {
  
  sm <- smooth_estimates(model, smooth = smooth_name) |>
    add_confint() |>
    arrange(days) |>
    mutate(
      effect = .lower_ci > 0 | .upper_ci < 0,
      run = cumsum(c(TRUE, diff(effect) != 0))
    )
  
  effect_runs <- sm |>
    filter(effect) |>
    group_by(run) |>
    summarise(
      start = min(days),
      end   = max(days),
      duration = end - start,
      direction = ifelse(mean(.estimate) > 0, "positive", "negative"),
      .groups = "drop"
    )
  
  #primary_run <- effect_runs |> slice(1)
  
  # list(
  #   recovery_day = primary_run$end,
  #   duration_days = primary_run$duration,
  #   direction = primary_run$direction,
  #   smooth_data = sm
  # )
  # list(
  #   recovery_day = effect_runs$end,
  #   duration_days = effect_runs$duration,
  #   direction = effect_runs$direction,
  #   smooth_data = sm
  # )
}

# import ------------------------------------------------------------------
source("R/models/01-gam-setup-NEW.R")

# keep only the files we need
rm(list = setdiff(ls(), c("model_in_toc_oa_f", "model_in_toc_oa_r", 
                          "model_in_uv_fin", "model_in_uv_raw",
                          "toc", "toc_oa", "uv")))

m_toc_r <- readRDS("outputs/models_NEW/m_toc_r.rds")
m_toc_f <- readRDS("outputs/models_NEW/m_toc_f.rds")

m_uv_r <- readRDS("outputs/models_NEW/m_uv_r.rds")
m_uv_f <- readRDS("outputs/models_NEW/m_uv_f.rds")
#m_uv_fin <- readRDS("outputs/models_NEW/m_uv_fin.rds")


# setup -------------------------------------------------------------------

pal <- wes_palette("Zissou1", type = "discrete")

# get flood_date
flood_date_numeric = as.numeric(as.Date("2023-07-24"))

model_in_toc_oa_r <- model_in_toc_oa_r %>%
  filter(days_since_flood < 101)

model_in_toc_oa_f <- model_in_toc_oa_f %>%
  filter(days_since_flood < 101)

# create pred grid --------------------------------------------------------
# create a new DF to generate predictions -- ONLINE ANLYZER
newdat_r <- model_in_toc_oa_r |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days = date_numeric - flood_date_numeric,
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
    # days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    year = lubridate::year(date),
    yday = lubridate::yday(date),
    type = "TOC, Raw"
  ) |>
  fitted_values(m_toc_r$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), exp))

newdat_f <- model_in_toc_oa_f |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days = date_numeric - flood_date_numeric,
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date),
    type = "TOC, Finished"
  ) |>
  fitted_values(m_toc_f$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), exp))

newdat_uv_r <- model_in_toc_oa_f |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days = date_numeric - flood_date_numeric,
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date),
    type = "UV254, Raw"
  ) |>
  fitted_values(m_uv_r$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), exp))

newdat_uv_f <- model_in_toc_oa_f |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    days = date_numeric - flood_date_numeric,
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date),
    type = "UV254, Finished"
  ) |>
  fitted_values(m_uv_f$gam, data = _, ci = 0.05) |>
  mutate(across(c(.fitted, .lower_ci, .upper_ci), exp))

newdat <- rbind(newdat_f, newdat_r, newdat_uv_r, newdat_uv_f)



# extract smooths ---------------------------------------------------------
# global smooth -----------------------------------------------------------

# global_smooth_r <- smooth_estimates(m_toc_r$gam, select = "s(date_numeric)") %>%
#   compute_sig() %>%
#   arrange(date_numeric) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          type = "Raw")
# 
# global_smooth_f <- smooth_estimates(m_toc_f$gam, select = "s(date_numeric)") %>%
#   compute_sig() %>%
#   arrange(date_numeric) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")),
#          type = "Finished")



# flood smooth ------------------------------------------------------------

flood_smooth_r <- smooth_estimates(m_toc_r$gam, select = "s(days)") %>%
  compute_sig() %>%
  arrange(days) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         type = "TOC, Raw")

flood_smooth_f <- smooth_estimates(m_toc_f$gam, select = "s(days)") %>%
  compute_sig() %>%
  arrange(days) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         type = "TOC, Finished")

flood_smooth_uv_r <- smooth_estimates(m_uv_r$gam, select = "s(days)") %>%
  compute_sig() %>%
  arrange(days) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         type = "UV254, Raw")

flood_smooth_uv_f <- smooth_estimates(m_uv_f$gam, select = "s(days)") %>%
  compute_sig() %>%
  arrange(days) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         type = "UV254, Finished")


# global_smooths <- rbind(global_smooth_f, global_smooth_r, global_smooth_) %>%
#   mutate(date = as.Date(date_numeric))

flood_smooths <- rbind(flood_smooth_f, flood_smooth_r, 
                       flood_smooth_uv_r, flood_smooth_uv_f)


# extract dates -----------------------------------------------------------

# toc_r_metrics      <- extract_disturbance_metrics(m_toc_r)
# toc_f_metrics      <- extract_disturbance_metrics(m_toc_f)
# uv_r_metrics      <- extract_disturbance_metrics(m_uv_r)
# uv_f_metrics      <- extract_disturbance_metrics(m_uv_f)

# disturbance_summary <- tibble(
#   Outcome = c("TOC Raw", "TOC Finished"),
#   Recovery_day = c(
#      toc_r_metrics$end,
#      toc_f_metrics$end
#   ),
#   Duration_days = c(
#      toc_r_metrics$duration,
#      toc_f_metrics$duration
#   ),
#   Direction = c(
#      toc_r_metrics$direction,
#      toc_f_metrics$direction
#   )
# )



#plot --------------------------------------------------------------------

# time series of observed values overlaid with fitted


a <- 
  newdat %>%
 # filter(type != "TOC, Finished") %>%
  mutate(
    param = case_when(
      type == "TOC, Raw" ~ "TOC (mg/L)",
      type == "UV254, Raw" ~ "UV254",
      type == "TOC, Finished" ~ "TOC (mg/L)",
      type == "UV254, Finished" ~ "UV254"
    ),
    location = case_when(
      type == "TOC, Raw" ~ "Raw",
      type == "UV254, Raw" ~ "Raw",
      type == "TOC, Finished" ~ "Finished",
      type == "UV254, Finished" ~ "Finished"
    )
  ) %>%
  ggplot(aes(x = date, y = .fitted)) +
  geom_line(aes(col = location),
            linewidth = 1) +
  scale_x_date(date_labels = "%b-%y") +
  facet_wrap(~param, ncol = 1,
             scales = "free_y",
             strip.position = "right") +
  scale_colour_manual(values = c(pal[2], pal[4])) +
  labs(col = "", y = "", x = "Date") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")


# seasonal smooth -----------------------------------------------------------
# prep
flood_smooths2 <- flood_smooths %>%
  mutate(flood_date = as.numeric(as.Date("2023-07-24")),
         param = case_when(
           type == "TOC, Raw" ~ "TOC (mg/L)",
           type == "UV254, Raw" ~ "UV254",
           type == "TOC, Finished" ~ "TOC (mg/L)",
           type == "UV254, Finished" ~ "UV254"
         ), location = case_when(
           type == "TOC, Raw" ~ "Raw",
           type == "UV254, Raw" ~ "Raw",
           type == "TOC, Finished" ~ "Finished",
           type == "UV254, Finished" ~ "Finished"
         ),
         param = factor(param, levels = c("TOC (mg/L)", "UV254")),
         location = factor(location, levels = c("Raw", "Finished"))
  ) #%>%
  #filter(type != "TOC, Finished")


b <- 
  ggplot() +
  geom_ribbon(data = flood_smooths2,
              aes(group = type, x = days, 
                  ymin = lower, ymax = upper), 
              alpha = 0.2, fill="grey70") +
  geom_line(data = flood_smooths2,
            aes(x = days, y = .estimate,
                col = factor(sig), group = sig_run)) +
  facet_wrap(param~location, ncol = 1,
             scales = "free_y",
             strip.position = "right") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  geom_vline(xintercept = 0, linetype = 2, 
             col = "grey") +
  labs(x="Days relative to event", y ="Partial effect", 
       col = "") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 12, face = "bold"))

b
#leg <- get_legend(b)


# get ECC data ------------------------------------------------------------

# source("R/figures/fig-eccdata.R")


# combine -----------------------------------------------------------------

a1 <- 
  a +
  geom_point(data = model_in_toc_oa_r %>% mutate(param = "TOC (mg/L)"),
             aes(x = date, y = toc),
             col = pal[4],
             size = 0.5, alpha = 0.4) +
  geom_point(data = model_in_toc_oa_r %>% mutate(param = "UV254"),
             aes(x = date, y = uv254),
             col = pal[4],
             size = 0.5, alpha = 0.4) +
  geom_point(data = model_in_toc_oa_f %>% mutate(param = "TOC (mg/L)"),
             aes(x = date, y = toc),
             col = pal[2],
             size = 0.5, alpha = 0.4) +
  geom_point(data = model_in_toc_oa_f %>% mutate(param = "UV254"),
             aes(x = date, y = uv254),
             col = pal[2],
             size = 0.5, alpha = 0.4) +
  geom_vline(xintercept = as.Date("2023-07-24"), linetype = 2, 
             col = "grey") +
  theme(text = element_text(size = 12, face = "bold"))




p1 <- ggarrange(a1, b, ncol = 2, widths = c(0.5, 0.5),
               labels = c("a", "b"),
          align = "v")
# 
# q <- ggarrange(p1, c_1, ncol = 1, 
#          # align = "hv",
#         #  padding = unit(0.1, "cm"),
#           heights = c(0.65, 0.35),
#         labels = c("", "c"))


ggsave("outputs/figures/fig-2-ts-toc-uv-model-smooths.png", p1,
       height = 6, width = 8, dpi = 600)
