# """
# This script generates Figure 1, which presents the main simulation results.
#
# It illustrates the finite-sample relative bias and coverage probability
# for the proposed distance-based shrinkage priors and two comparison methods,
# as described in the main text.
# """

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

source("pkg/utilities.R")

# load results by averaging across replications
simtab <- empirical_results(indir = "output/reps") %>%
  filter(T_0 == 30, J == 50, mu_d == 0) %>%
  pivot_longer(cols = c(pct_bias, coverage), names_to = "property", values_to = "value") %>%
  select(name, kappa_d, spillover, property, value) %>%
  mutate(
    kappa_d = number(kappa_d, accuracy = 0.1),
    property = recode_factor(property, "pct_bias" = "Relative Bias (%)", "coverage" = "Coverage Probability (%)")
  )

# figure 1: simulation results for relative bias and coverage probability
p1 <- ggplot(simtab, mapping = aes(x = spillover, y = value, color = name)) +
  geom_hline(
    data = simtab %>% filter(property == "Coverage Probability (%)"),
    mapping = aes(yintercept = 0.95),
    linetype = "dashed"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_grid(
    property ~ kappa_d,
    labeller = label_bquote(cols = kappa[italic(d)]==.(kappa_d)),
    scales = "free_y"
  ) +
  scale_color_manual(
    name = "",
    breaks = c("DHS", "DS2", "BSTS", "GSC"),
    labels = c(
      "Distance Horseshoe Prior",
      "Distance Spike-and-Slab Prior",
      "Bayesian Structural Time-Series Model",
      "Generalized Synthetic Control Method"
    ),
    values = c("#fc8d62", "#66c2a5", "#8da0cb", "#e78ac3")
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(breaks = breaks_extended(n = 8), labels = percent) +
  labs(x = "Proportion of Control Units Impacted by Spillover (%)", y = "") +
  theme_bw(base_size = 10) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    legend.position = "bottom",
    panel.spacing = unit(5, "mm"),
    strip.background =element_rect(fill = "grey90")
  )

save_plot("figs/fig01.pdf", plot = p1, width = 9, height = 6)
