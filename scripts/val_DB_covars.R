# """
# This script generates Figures S4-S5, which presents auxiliary experiments.
#
# It illustrates the finite-sample relative bias and coverage probability
# for the proposed distance-based shrinkage priors and two comparison methods,
# as described in the supplementary. This setting differs from the primary
# experiments by introducing distance-dependent covariates.
# """

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

source("pkg/utilities.R")

# load results by averaging across replications
simtab <- empirical_results(indir = "output/reps") %>%
  filter(T_0 == 30, J == 50) %>%
  pivot_longer(cols = c(pct_bias, coverage), names_to = "property", values_to = "value") %>%
  select(name, kappa_d, mu_d, spillover, property, value) %>%
  mutate(
    kappa_d = number(kappa_d, accuracy = 0.1),
    mu_d = number(mu_d, accuracy = 0.1),
    property = recode_factor(property, "pct_bias" = "Relative Bias (%)", "coverage" = "Coverage Probability (%)")
  )

# figure S2: simulation results for relative bias
p2 <- filter(simtab, property == "Relative Bias (%)") %>%
  ggplot(simtab, mapping = aes(x = spillover, y = value, color = name)) +
  geom_line() +
  geom_point(size = 1.75) +
  facet_grid(
    mu_d ~ kappa_d,
    labeller = label_bquote(cols = kappa[italic(d)]==.(kappa_d), rows = mu[italic(d)]==.(mu_d)),
    scales = "fixed"
  ) +
  scale_color_manual(
    name = "",
    breaks = c("BSTS", "DHS", "DS2", "GSC"),
    labels = c(
      "Bayesian Structural Time-Series Model",
      "Distance Horseshoe Prior",
      "Distance Spike-and-Slab Prior",
      "Generalized Synthetic Control Method"
    ),
    values = c("#8da0cb", "#fc8d62", "#66c2a5", "#e78ac3")
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

save_plot("sfigs/sfig04.pdf", plot = p2, width = 12, height = 8)

# figure S3: simulation results for coverage probability
p2 <- filter(simtab, property == "Coverage Probability (%)") %>%
  ggplot(simtab, mapping = aes(x = spillover, y = value, color = name)) +
  geom_hline(
    data = simtab %>% filter(property == "Coverage Probability (%)"),
    mapping = aes(yintercept = 0.95),
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point(size = 1.75) +
  facet_grid(
    mu_d ~ kappa_d,
    labeller = label_bquote(cols = kappa[italic(d)]==.(kappa_d), rows = mu[italic(d)]==.(mu_d)),
    scales = "fixed"
  ) +
  scale_color_manual(
    name = "",
    breaks = c("BSTS", "DHS", "DS2", "GSC"),
    labels = c(
      "Bayesian Structural Time-Series Model",
      "Distance Horseshoe Prior",
      "Distance Spike-and-Slab Prior",
      "Generalized Synthetic Control Method"
    ),
    values = c("#8da0cb", "#fc8d62", "#66c2a5", "#e78ac3")
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

save_plot("sfigs/sfig05.pdf", plot = p2, width = 12, height = 8)
