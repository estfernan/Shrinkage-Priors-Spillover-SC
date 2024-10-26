# """
# This script generates Figure 2, which presents the main application results.
#
# It illustrates the effect of Philadelphia's beverage tax on sales of
# sugar-sweetened and artificially sweetened beverages at mass merchandise
# stores in the city. The estimates are obtained using the distance-based
# shrinkage priors with weighted distances under various importance weights.
# """

library(dplyr)
library(ggplot2)
library(latex2exp)
library(scales)
library(tidyr)

source("pkg/utilities.R")

load("data/sample_information.RData")

draws_out <- readRDS("output/beverage_tax_causal_effect_estimates.RDS")

# compute posterior estimates
post_info <- draws_out %>%
  summarise(
    mean = mean(draw, na.rm = TRUE),
    lwr_CI = quantile(draw, p = 0.025, na.rm = TRUE),
    upp_CI = quantile(draw, p = 0.975, na.rm = TRUE),
    .by = c(prior, kappa_d, other)
  )

p1 <- ggplot(post_info, mapping = aes(x = other, y = mean, ymin = lwr_CI, ymax = upp_CI, color = prior)) +
  geom_vline(xintercept = dates[T0], linetype = "dashed") +
  geom_errorbar() +
  geom_line() +
  geom_point(size = 1.75) +
  facet_grid(
    ~ kappa_d,
    labeller = label_bquote(cols = kappa[italic(d)]==.(kappa_d)),
    scales = "free_y"
  ) +
  scale_color_manual(
    name = "",
    breaks = c("DHS", "DS2"),
    labels = c("Distance Horseshoe Prior", "Distance Spike-and-Slab Prior"),
    values = c("#66c2a5", "#fc8d62")
  ) +
  scale_x_date() +
  scale_y_continuous(labels = comma) +
  labs(
    x = TeX("Aggregated Four-Week Sales Reporting Period"),
    y = TeX("Change in Relative Volume Sales in Ounces, $\\tau_t$")
  ) +
  theme_bw(base_size = 12) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 12, 0, 0)),
    legend.position = "bottom",
    panel.spacing = unit(5, "mm"),
    strip.background =element_rect(fill = "grey90")
  )

save_plot("figs/fig02.pdf", plot = p1, width = 12, height = 9)

