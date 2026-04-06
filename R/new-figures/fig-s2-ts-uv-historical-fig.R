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

source("R/figures/figure-helpers.R")
#m_toc <- readRDS("outputs/models_NEW/m_toc.rds")
m_uv_fin <- readRDS("outputs/models_NEW/m_uv_fin.rds")
m_uv_raw <- readRDS("outputs/models_NEW/m_uv_raw.rds")

model_in_uvr <- readRDS("data/model-in/model_in_uvr.rds")
model_in_uvf <- readRDS("data/model-in/model_in_uvf.rds")

# setup -------------------------------------------------------------------

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3


# get flood_date
flood_date_numeric = as.numeric(as.Date("2023-07-24"))

# create a new DF to generate predictions
###FINISHED
newdat_f <- model_in_uvf |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    yday = yday(date),
    days_since_flood = date_numeric - flood_date_numeric,
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days_since_flood > 0, 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date)
  ) |>
  fitted_values(m_uv_fin$gam, data = _, ci = 0.05)

###RAW
newdat_r <- model_in_uvr |>
  nest() |>
  mutate(
    date_numeric = map(data, ~ with(.x, seq(min(date_numeric),  max(date_numeric))))
  ) |>
  unnest(date_numeric) |>
  arrange(date_numeric) |>
  mutate(
    date = as.Date(date_numeric),
    yday = yday(date),
    days_since_flood = date_numeric - flood_date_numeric,
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood), 
    post_flood = ifelse(days_since_flood > 0, 1, 0),
    year = lubridate::year(date),
    yday = lubridate::yday(date)
  ) |>
  fitted_values(m_uv_raw$gam, data = _, ci = 0.05)




# get smooths -------------------------------------------------------------

global_smooth_fin <- smooth_estimates(m_uv_fin$gam, select = "s(date_numeric)") %>%
  compute_sig() %>%
  arrange(date_numeric) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")))

global_smooth_raw <- smooth_estimates(m_uv_raw$gam, select = "s(date_numeric)") %>%
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

seasonal_smooth_fin <- smooth_estimates(m_uv_fin$gam, select = "s(yday)") %>%
  compute_sig() %>%
  arrange(yday) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")))

seasonal_smooth_raw <- smooth_estimates(m_uv_raw$gam, select = "s(yday)") %>%
  compute_sig() %>%
  arrange(yday) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")))


#plot --------------------------------------------------------------------
  
newdat_f$location = "Finished"
newdat_r$location = "Raw"

newdat <- rbind(newdat_f, newdat_r)
  # time series of observed values overlaid with fitted
  

p1 <- ggplot() +
  geom_ribbon(data = newdat, #%>% filter(year=="2023"),
              aes(x = date, 
                  ymin = exp(.lower_ci), ymax = exp(.upper_ci)),
              alpha = 0.4) +
  geom_line(data = newdat, #%>% filter(year=="2023"),
            aes(x = date, y = exp(.fitted), 
                )) +
  scale_x_date(date_labels = "%b-%Y") +
 # scale_colour_manual(values = c("black", pal2[2])) +
  labs(col = "", y = "UV254", x = "Date") +
  facet_wrap(~location, scales = "free_y", ncol = 1,
             strip.position = "right") +
  theme_bw() +
  theme(legend.position = "top")



# global smooth -----------------------------------------------------------

global_smooth_fin$location = "Finished"
global_smooth_raw$location = "Raw"


global_smooth <- rbind(global_smooth_fin, global_smooth_raw) %>%  
  mutate(date = as.Date(date_numeric),
         location = factor(location, levels = c("Raw", "Finished"))) #%>%
  #filter(date > as.Date("2022-12-31"))

p2 <- 
  global_smooth %>%
  mutate(smooth = "Global Smooth") %>%
  mutate(date = as.Date(date_numeric)) %>%
  ggplot(aes(x = date, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(smooth~location, scales = "free_y", ncol = 1,
             strip.position = "right") +
 # facet_wrap(~smooth, strip.position = "right") +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", legend.text = element_text(size = 10))

# seasonal smooth -----------------------------------------------------------
seasonal_smooth_fin$location = "Finished"
seasonal_smooth_raw$location = "Raw"


seasonal_smooth <- rbind(seasonal_smooth_fin, seasonal_smooth_raw) %>%  
  mutate(location = factor(location, levels = c("Raw", "Finished"))) 


p3 <- 
  seasonal_smooth %>%
  mutate(smooth = "Seasonal Smooth") %>%
  ggplot(aes(x = yday, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(smooth~location, ncol = 1, scales = "free_y",
             strip.position = "right") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  scale_x_continuous(breaks = c(2, 60, 121, 182, 243, 304),
                     labels = c("Jan", "Mar", "May", "July", "Sept", "Nov")
                     ) +
  labs(x="", y = "",#y="Partial effect", 
       col = "") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", legend.text = element_text(size = 10))
  
# combine -----------------------------------------------------------------

uv <- rbind(model_in_uvf, model_in_uvr)

p1 <- p1 +
  geom_point(data = uv, #%>% filter(year == 2023,
                         #                  date > as.Date("2022-12-31")),
             aes(x = date, y = uv), #group = series, col = factor(lsl)),
             alpha = 0.2, size = 0.5) +
  geom_vline(xintercept = as.Date("2023-07-24"), linetype = "dashed")

q1 <- ggarrange(p2, p3,ncol = 1, align = "v",
          labels = c("B", "C"))

q <- ggarrange(NULL, p1, q1, NULL, ncol = 4, widths = c(0.05, 0.5, 0.45, 0.05),
               labels = c("A", ""))


ggsave("outputs/figures/fig-s1-uv_historical-seasonal_effect.png", q,
       height = 6, width = 12, dpi = 600)
