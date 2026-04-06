source("R/models/02-gam-pb-mech.R")
source("R/figures/figure-helpers.R")
library(data.table)

# theme -------------------------------------------------------------------

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal2 <- palette[c(1,6)]
alpha <- .3

pal <- wes_palette("Zissou1", type = "discrete")



# extract smooth ----------------------------------------------------------

smooth_toc0 <- smooth_estimates(m_pb_mech, smooth = c("s(toc_oa)")) %>%
  compute_sig() %>%
   arrange(toc_oa) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Partial effect, TOC")

smooth_fe <- smooth_estimates(m_pb_mech, smooth = c("s(log_fe)")) %>%
  compute_sig() %>%
  arrange(log_fe) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Partial Effect, Iron")

smooth_al <- smooth_estimates(m_pb_mech, smooth = c("s(log_al)")) %>%
  compute_sig() %>%
  arrange(log_al) %>%
  mutate(sig_run = rleid(sig),
         sig = factor(sig, levels = c("-1", "0", "1"),
                      labels = c("<0", "0", ">0")),
         smooth = "Partial Effect, Aluminum")

# plot --------------------------------------------------------------------


#a <- 
a <- smooth_toc0 %>%
  # mutate(date = as.Date(date_numeric)) %>%
  ggplot(aes(x = toc_oa, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  facet_wrap(~smooth,
             strip.position = "right") +
  # scale_x_date(date_labels = "%b-%Y", breaks = "4 months") +
  scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  labs(x="TOC (mg/L)", y = "",#y="Partial effect", 
       col = "d[Pb]/dt") +
  theme_bw() +
  theme(legend.position = "top", 
        text = element_text(size = 12, face = "bold"))

#b <- 
  b <- smooth_fe %>%
  # mutate(date = as.Date(date_numeric)) %>%
  ggplot(aes(x = exp(log_fe), y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
  geom_line() +
  geom_line(aes(col = factor(sig), group = sig_run)) +
  scale_x_log10() +
  facet_wrap(~smooth,
             strip.position = "right") +
  # scale_x_date(date_labels = "%b-%Y", breaks = "4 months") +
    scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
  labs(x="Fe [μg/L]", y = "",#y="Partial effect",
       col = "d[Pb]/dt") +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size = 12, face = "bold"))
  
  c <- smooth_al %>%
    # mutate(date = as.Date(date_numeric)) %>%
    ggplot(aes(x = exp(log_al), y = .estimate)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey70") +
    geom_line() +
    geom_line(aes(col = factor(sig), group = sig_run)) +
    scale_x_log10() +
    facet_wrap(~smooth,
               strip.position = "right") +
    # scale_x_date(date_labels = "%b-%Y", breaks = "4 months") +
    scale_colour_manual(values = c(pal2[1], "black", pal2[2])) +
    labs(x="Al [μg/L]", y = "",#y="Partial effect",
         col = "d[Pb]/dt") +
    theme_bw() +
    theme(legend.position = "top",
          text = element_text(size = 12, face = "bold"))

fig <- ggarrange(a, b, c, ncol = 3, common.legend = TRUE)

ggsave(filename = "outputs/figures/fig5-pb-mech-smooths.png", fig,
       dpi = 600, width = 8, height = 8)
