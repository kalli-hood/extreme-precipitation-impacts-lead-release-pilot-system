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
library(ggpubr)


# import ------------------------------------------------------------------
source("R/models/01-gam-setup-NEW.R")
rm(list = setdiff(ls(), c("model_in_toc_oa_f", "model_in_toc_oa_r", 
                          "model_in_uv_fin", "model_in_uv_raw",
                          "toc", "toc_oa", "uv")))
source("R/figures/figure-helpers.R")
m_toc <- readRDS("outputs/models_NEW/m_toc.rds")

# setup -------------------------------------------------------------------

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3


# get flood_date
flood_date_numeric = as.numeric(as.Date("2023-07-24"))

# create a new DF to generate predictions

newdat <- toc |>
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
  fitted_values(m_toc$gam, data = _, ci = 0.05)


global_smooth <- smooth_estimates(m_toc$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")))
# mutate(lower = .estimate - 1.96*.se,
#        upper = .estimate + 1.96*.se,
#        # date = as.Date(date_numeric),
#        sig = ifelse(lower > 0, 1,
#                     ifelse(upper < 0, -1, 0))
# )

seasonal_smooth <- smooth_estimates(m_toc$gam, select = "s(yday)") %>%
  compute_sig() %>%
  arrange(yday) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")))


# flood_smooth <- smooth_estimates(m_toc$gam, select = "s(days_since_flood)") %>%
#   compute_sig() %>%
#   arrange(days_since_flood) %>%
#   mutate(sig_run = rleid(sig),
#          sig = factor(sig, levels = c("-1", "0", "1"),
#                       labels = c("<0", "0", ">0")))

#toc_metrics      <- extract_disturbance_metrics(m_toc)

# disturbance_summary <- tibble(
#   Outcome = c("TOC"),
#   Recovery_day = c(
#     # uv_raw_metrics$recovery_day,
#     # uv_finished_metrics$recovery_day
#      toc_metrics$recovery_day
#   ),
#   Duration_days = c(
#     # uv_raw_metrics$duration_days,
#     # uv_finished_metrics$duration_days
#      toc_metrics$duration_days
#   ),
#   Direction = c(
#     # uv_raw_metrics$direction,
#     # uv_finished_metrics$direction
#      toc_metrics$direction
#   )
# )

# 
# days_fs <- smooth_estimates(m_pb$gam, select = "s(date_numeric,series)") %>%
#   mutate(days_rel = days,
#          switch = ifelse(days_rel < 0, -1, 1))
# 
# # fs_df <- bind_rows(pre_fs, post_fs) %>%
# fs_df <- days_fs %>%
#   mutate(
#     replicate = factor(str_sub(series, -1)),
#     material = factor(str_sub(series, 1, -2)),
#     main = case_when(
#       replicate %in% c(1,3,5) ~ "CI",
#       TRUE ~ "PVC"
#     )
#   ) %>%
#   compute_sig() %>%
#   add_sig_runs()

#plot --------------------------------------------------------------------
  
  # time series of observed values overlaid with fitted
  

p1 <- ggplot() +
  geom_ribbon(data = newdat, #%>% filter(year=="2023"),
              aes(x = date, 
                  ymin = exp(.lower_ci), ymax = exp(.upper_ci)),
              alpha = 0.4) +
  geom_line(data = newdat, #%>% filter(year=="2023"),
            aes(x = date, y = exp(.fitted), 
                )) +
  scale_x_date(date_labels = "%b-%y") +
 # scale_colour_manual(values = c("black", pal2[2])) +
  labs(col = "", y = "TOC (mg/L)", x = "Date") +
  theme_bw() +
  theme(legend.position = "top")

#global smooth

global_smooth2 <- global_smooth %>%  
  mutate(date = as.Date(date_numeric)) #%>%
  #filter(date > as.Date("2022-12-31"))

p2 <- global_smooth2 %>%
  mutate(smooth = "Global Smooth") %>%
  mutate(date = as.Date(date_numeric)) %>%
  ggplot(aes(x = date, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(~smooth, strip.position = "right") +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  scale_colour_manual(values = c(pal2[2], "black", pal2[1])) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", legend.text = element_text(size = 10))

# seasonal smooth -----------------------------------------------------------

p3 <- seasonal_smooth %>%
  mutate(smooth = "Seasonal Smooth") %>%
  ggplot(aes(x = yday, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(~smooth, strip.position = "right") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  scale_x_continuous(breaks = c(2, 60, 121, 182, 243, 304),
                     labels = c("Jan", "Mar", "May", "July", "Sept", "Nov")
                     ) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", legend.text = element_text(size = 10))
  
# seasonal smooth -----------------------------------------------------------


# p4 <- flood_smooth %>%
#   mutate(flood_date = as.numeric(as.Date("2023-07-24")),
#          date = as.Date(flood_date + days_since_flood)) %>%
#     ggplot(aes(x = date, y = .estimate)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
#   geom_line() +
#   geom_line(aes(col = factor(sig), group = sig_run)) +
#   scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
#   scale_x_date(date_labels = "%b-%Y", breaks = "3 months") +
#   # scale_x_continuous(breaks = c(90, 182, 274),
#   #                    labels = c("April", "July", "October")) +
#   labs(x="", y = "",#y="Partial effect", 
#        col = "d[TOC]/dt") +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "none", legend.text = element_text(size = 10))

#legend <- get_legend(p4)
# factor smooths -----------------------------------------------------------


# fs_pb <- ggplot(fs_df, 
#                 aes(x = days_rel, y = .estimate)) +
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper),
#               alpha = 0.1) +
#   geom_line() +
#   geom_line(data = subset(fs_df, sig == 1),
#             aes(x = days_rel, y = .estimate, group = interaction(series, sig_run)),
#             color = "red", linewidth = 0.6) +
#   geom_vline(xintercept = 0, linetype="dashed") +
#   facet_wrap(~series) +
#   theme_minimal()




# combine -----------------------------------------------------------------

p1 <- p1 +
  geom_point(data = toc, #%>% filter(year == 2023,
                         #                  date > as.Date("2022-12-31")),
             aes(x = date, y = toc), #group = series, col = factor(lsl)),
             alpha = 0.4) +
  geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")

q1 <- ggarrange(p2, p3,ncol = 1, align = "v",
          labels = c("B", "C"))

q <- ggarrange(NULL, p1, q1, NULL, ncol = 4, widths = c(0.05, 0.55, 0.35, 0.05),
               labels = c("A", ""))


ggsave("outputs/figures/fig-s1-toc_duration_effect.png", q,
       height = 6, width = 12, dpi = 600)
