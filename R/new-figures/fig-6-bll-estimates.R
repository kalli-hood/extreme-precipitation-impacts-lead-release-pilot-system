# -------------------------------------------------------------------------
# 05_make_figures_optionB.R
#
# Purpose:
#   Create publication-ready figures from Option B Monte Carlo outputs
#
# Inputs:
#   - bll_optionB_daily_summary.rds
#   - pb_daily_clean.rds
#   - results_period_table.csv (optional)
#
# Outputs:
#   - Figures saved to figures/
#
# Notes:
#   - Uses summaries only (memory-safe)
#   - Designed for easy iteration / debugging
# -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)


palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3

pal <- wes_palette("Zissou1", type = "discrete")



# -------------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------------

daily_summary <- readRDS("data/processed/bll_daily_summary.rds")

pb_daily <- readRDS("data/processed/pb_pred_daily_HRA.rds")

flood_dates <- tribble(
  ~start,                              ~end,
  as.numeric(as.Date("2024-07-24")), as.numeric(as.Date("2024-07-24") +80)
) |>
  mutate(start = as.Date(start), end = as.Date(end))
  
# -------------------------------------------------------------------------
# Helper: identify flood window (if encoded as period)
# -------------------------------------------------------------------------

flood_dates <- daily_summary %>%
  filter(period == "Disturbance") %>%
  summarise(
    start = min(date),
    end   = max(date)
  )

# -------------------------------------------------------------------------

# choose a small number of age bins for clarity
main_age_bins <- daily_summary %>%
  distinct(min_age_yr) %>%
  arrange(min_age_yr) %>%
  slice(3,#8,
        13,28,
       # 43,
        58 #,73
       ) %>%        # adjust as needed
  pull(min_age_yr)

# probability mapping -------------------------------------------------

fig1 <- daily_summary %>%
  filter(
    fraction_from_system == 0.5,
    pb_quantile %in% c("p25", "p50", "p75")
  ) %>%
  mutate(
    scenario = case_when(
      pb_quantile == "p25" ~ "low",
      pb_quantile == "p50" ~ "mid",
      pb_quantile == "p75" ~ "high"
    )
  ) %>%
  select(date, min_age_yr, scenario, bll_mean:p_exceed) 


# Select which age bins to show (2–3 max)
main_age_bins <- fig1 %>%
  distinct(min_age_yr) %>%
  arrange(min_age_yr) %>%
  slice(3,#8,
        13,28,
        # 43,
        58 #,73
  ) %>%
  pull(min_age_yr)

plot_data <- fig1 %>%
  filter(min_age_yr %in% main_age_bins) %>%
  pivot_wider(
    names_from = scenario,
    values_from = bll_mean:p_exceed
  ) 
start_shade <- ymd("2023-07-24")
end_shade <- ymd("2023-10-12")

a <- plot_data %>%
 # filter(date < as.Date("2024-01-01")) %>%
  mutate(age = paste0("Age:", min_age_yr, "yr"),
         age = factor(age, levels = c("Age:5yr", 
                                      "Age:15yr", 
                                      "Age:30yr", 
                                      "Age:60yr"))) %>%
  ggplot() +
  geom_rect(aes(xmin = start_shade, xmax = end_shade, 
                ymin = 0.3, ymax = Inf), 
            fill = "gray90", alpha = 0.5) +
  geom_ribbon(aes(x = date,
                  ymin = bll_p05_low,
                  ymax = bll_p95_high),
              col = palette[2],
              fill = palette[2],
             # linetype = 2,
              alpha = 0.3
              ) +
  geom_ribbon(aes(x = date,
                  ymin = bll_p50_low, 
                  ymax = bll_p50_high),
              col = palette[1],
              fill = palette[1],
              alpha = 0.4
              ) +
  geom_line(aes(x = date,
                y = bll_p50_mid),
   linewidth = 1) +
  geom_hline(yintercept = 3.5, col = "darkred", linetype = 2)+
  facet_wrap(~ age, ncol = 1) +
  scale_x_date(date_labels = "%b-%y") +
  scale_y_log10() +
  labs(
    x = NULL,
    y = "Predicted blood lead (µg/dL)") +
  theme_bw() +
  theme(text = element_text(size = 12, face = "bold"))




# heatmap -----------------------------------------------------------------

heatmap_data <- daily_summary %>%
  filter(period == "Disturbance") %>%
  filter(
    min_age_yr %in% main_age_bins
  #  pb_quantile %in% c("p50", "p95"),
  #  fraction_from_system %in% c(0.25, 0.5, 0.75)
  ) %>%
  group_by(
    min_age_yr,
    pb_quantile,
    fraction_from_system
  ) %>%
  summarise(
    mean_p_exceed = mean(p_exceed),
    .groups = "drop"
  )

zissou_cols <- wes_palette("Zissou1", n = 256, type = "continuous")

b <- heatmap_data %>%
  mutate(age = paste0("Age:", min_age_yr, "yr"),
         age = factor(age, levels = c("Age:5yr", 
                                      "Age:15yr", 
                                      "Age:30yr", 
                                      "Age:60yr"))) %>%
  ggplot(
  aes(
    x = factor(fraction_from_system),
    y = pb_quantile,
    fill = mean_p_exceed
  )
) +
  geom_tile() +
  facet_wrap(~ age, ncol = 1) +
  scale_fill_gradientn(
    colours = zissou_cols,
    values  = scales::rescale(c(0.1, 0.2, 0.5, 0.8, 1)),
    limits  = c(0, 1),
   # oob     = squish,
    name    = "Mean exceedance\nprobability"
  ) +
  labs(
    x = "Fraction of water from system",
    y = "Water Pb percentile"
   # title = "Sensitivity of predicted risk to exposure scenarios (flood period)"
  ) +
  theme_bw() +
  theme(legend.position = "none", 
        text = element_text(size = 12, face = "bold"))

#leg <- get_legend(b)




c <- ggarrange(a, b, align = "hv")
d <- ggarrange(NULL, leg, ncol = 2, labels = c("a", "b"))

fig <- ggarrange(d, c, ncol =1, heights = c(0.1, 0.9))


ggsave(filename = "outputs/figures/fig-6-bll-estimates.png", fig,
       dpi = 600, width = 8, height = 10)
