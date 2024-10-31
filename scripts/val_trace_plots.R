# """
# This script generates Figure S5, which presents a set of trace plots.
# """

library(dplyr)
library(ggplot2)
library(latex2exp)
library(scales)

source("pkg/utilities.R")

load("data/sample_information.RData")

set.seed(123, kind = "L'Ecuyer-CMRG")

draws_out <- readRDS("output/beverage_tax_causal_effect_estimates.RDS")

chain_idx <- sample(unique(draws_out$chain), size = 1)
set_dates <- sample(dates[-seq_len(T0)], size = 4)

# subset for randomly selected chains and estimators
mcmc_iters <- draws_out %>%
  filter(
    kappa_d == 0.0,
    chain == chain_idx,
    other %in% set_dates
  ) %>%
  mutate(
    prior = recode_factor(
      prior,
      "DHS" = "Distance Horseshoe Prior",
      "DS2" = "Distance Spike-and-Slab Prior"
    )
  )

# figure S5: trace plots for a randomly selected subset of estimators
p1 <- ggplot(mcmc_iters, mapping = aes(x = iter, y = draw, color = prior)) +
  geom_line() +
  facet_grid(other ~ prior, scales = "free") +
  scale_color_manual(
    name = "",
    breaks = c("Distance Horseshoe Prior", "Distance Spike-and-Slab Prior"),
    values = c("#fc8d62", "#66c2a5")
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    x = TeX("Post-warmup Iteration, $r$"),
    y = TeX("Posterior Draw, $\\tau_t^{(r)}$")
  ) +
  theme_bw(base_size = 10) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 12, 0, 0)),
    legend.position = "none",
    panel.spacing = unit(5, "mm"),
    strip.background =element_rect(fill = "grey90")
  )

save_plot("sfigs/sfig05.pdf", plot = p1, width = 14, height = 9)
