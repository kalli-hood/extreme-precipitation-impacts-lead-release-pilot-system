## This is a setup file, intended to be called from one of the 
## scripts making figures

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



# import ------------------------------------------------------------------
#source("R/models/01-gam-setup-NEW.R")
source("R/figures/figure-helpers.R")
model_in_pb <- readRDS("data/model-in/model_in_pb.rds")
m_pb <- readRDS("outputs/models_NEW/m_pb.rds")
m_pb_duration <- readRDS("outputs/models_NEW/m_pb_duration.rds")


# theme -------------------------------------------------------------------

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3

pal <- wes_palette("Zissou1", type = "discrete")

# setup -------------------------------------------------------------------

# get flood_date
flood_date_numeric = as.numeric(as.Date("2023-07-24"))

# create a new DF to generate predictions
newdat <- model_in_pb |>
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



global_smooth <- smooth_estimates(m_pb$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Global Smooth")

seasonal_smooth <- smooth_estimates(m_pb$gam, select = "s(yday)") %>%
  compute_sig() %>%
  arrange(yday) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Seasonal Smooth")


# disturbance analysis ----------------------------------------------------


model_in_pb2 <- model_in_pb %>%
  mutate(flood = factor(ifelse(post_flood == 0, -1, 1)),
         days = date_numeric - flood_date_numeric,
         days_rel = ifelse(post_flood == 1, days_since_flood, NA))
pb_window <- subset(model_in_pb2, days > -150 & days < 150)

flood_smooth <- smooth_estimates(m_pb_duration$gam, select = "s(days)") %>%
  compute_sig() %>%
  arrange(days) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Flood")

#pb_metrics      <- extract_disturbance_metrics2(m_pb_duration)




#plot --------------------------------------------------------------------
  
  # time series of observed values overlaid with fitted
  

a <- 
  ggplot() +
  geom_point(data = model_in_pb %>% filter(year == 2023,
                                           date > as.Date("2023-05-01")),
             aes(x = date, y = value, group = series, col = factor(lsl)),
             size = 0.5,
             # linewidth = 0.5,
             alpha = 0.4) +
   geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed") +
  geom_line(data = newdat %>% filter(year == 2023,
                                     date > as.Date("2023-06-01")),
            aes(x = date, y = .fitted, 
                group = factor(series),col = factor(lsl))) +
  facet_wrap( ~ replicate,
             ncol = 2,
            labeller = labeller(replicate = as_labeller(
              c(
                "1" = "Rack 1 (Cast Iron)",
                "2" = "Rack 2 (PVC)",
                "3" = "Rack 3 (Cast Iron)",
                "4" = "Rack 4 (PVC)",
                "5" = "Rack 5 (Cast Iron)",
                "6" = "Rack 6 (PVC)"
              )
            ))) +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 1000),
    labels = c("0.1", "1", "10", "100", "1000")
  ) +
  scale_x_date(date_labels = "%b-%Y") +
  scale_color_manual(labels = c("Pb", "Cu-Pb"), values = pal2) +
  labs(col = "", y = "Pb (µg/L)", x = "Date") +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size = 12, face = "bold"))
a
#global smooth

b <- 
  global_smooth %>%
  mutate(date = as.Date(date_numeric)) %>%
  ggplot(aes(x = date, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(~smooth, 
             strip.position = "right") +
  scale_x_date(date_labels = "%b-%y", breaks = "4 months") +
  scale_colour_manual(values = c("black", pal2[2])) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 12, face = "bold"))

b
# seasonal smooth -----------------------------------------------------------

c <- 
    ggplot(seasonal_smooth,
                aes(x = yday, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(~smooth, 
             strip.position = "right") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  scale_x_continuous(breaks = c(2, 60, 121, 182, 243, 334),
                     labels = c("January", "March", "May", "July", "September", "December")) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 12, face = "bold"))

  c
# flood smooth -----------------------------------------------------------
## PLOT DISTURBANCE

d <- 
    flood_smooth %>%
  ggplot(aes(x = days, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              fill = "grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap( ~ smooth, strip.position = "right") +
  scale_x_continuous(breaks = c(-150, -100, -50, 0, 50, 100, 150)) +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  labs(
    x = "Days relative to event",
    y = "",
    #y="Partial effect",
    col = ""
  ) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 12, face = "bold"))



# all smooths -------------------------------------------------------------




# combine -----------------------------------------------------------------


q0 <- ggarrange(NULL, b, c, #d, 
                ncol = 1, align = "v", heights = c(0.1, 0.45, 0.45),
          labels = c("b", "", "c"))


q <- ggarrange(a, NULL, q0, ncol =3, align = "hv",
               widths = c(0.6, 0.05, 0.33),
               labels = c("a", ""))


ggsave("outputs/figures/fig-4-pb_duration_effect-v2.png", q,
       height = 6, width = 12, dpi = 600)


ggsave("outputs/figures/fig-s4-flood-duration-pb.png", d,
       height = 4, width =5, dpi = 600)
