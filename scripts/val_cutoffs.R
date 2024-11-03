# """
# This script generates Figure S6, which presents auxiliary application results.
#
# It illustrates the effect of Philadelphia's beverage tax across different
# values for the cutoff used in the distance-based spike-and-slab prior.
# An importance weight of zero is used to represent the cutoff solely as a
# geographical distance from the treated unit.
# """

library(colorspace)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(scales)

source("pkg/models.R")
source("pkg/ops.R")
source("pkg/private.R")
source("pkg/utilities.R")

load("data/sample_information.RData")

set.seed(123, kind = "L'Ecuyer-CMRG")

kappa_d    <- 0
n_chains   <- 4
n_burn     <- 5000
n_iter     <- 5000
save_draws <- TRUE
pct_list   <- seq(from = 0, to = 0.5, by = 0.05)

if (!exists("beverage_sales"))
{
  beverage_sales <- readRDS("data/sample_sales.RDS")
  warning("FileNotFound: R Object 'beverage_sales' not found, loading sample data.")
}

std <- standardize_matrix(beverage_sales, train = seq_len(T0))
X   <- scale(baseline_covariates)
d_C <- weighted_distance(X, d, kappa_d = kappa_d)

Y <- std[["x"]]

draws_out <- data.frame()

for (pct in pct_list)
{
  rho <- quantile(d_C, p = pct, names = FALSE)

  tau_DS2 <- db_bayesian_sc(
    Y, T0, d = d_C, rho = rho,
    prior = "DS2",
    refresh = 1000,
    chains = n_chains,
    parallel_chains = 4,
    iter_warmup = n_burn, iter_sampling = n_iter,
    max_treedepth = 12, adapt_delta = 0.99
  )

  tau_DS2 <- std$s_hat[1] * tau_DS2

  ds2tab <- multiple_chains_to_dataframe(
    tau_DS2,
    other_info = data.frame(prior = "DS2", kappa_d = kappa_d, percent = pct),
    pivot = TRUE,
    extra_ids = dates,
    cols = starts_with("tau"),
    names_to = "estimator",
    values_to = "draw"
  )

  draws_out <- rbind(draws_out, ds2tab)
}

if (save_draws)
{
  saveRDS(draws_out, file = "output/sensitivity_analysis_beverage_tax.RDS")
}

# compute posterior estimates
post_info <- draws_out %>%
  summarise(
    mean = mean(draw, na.rm = TRUE),
    lwr_CI = quantile(draw, p = 0.025, na.rm = TRUE),
    upp_CI = quantile(draw, p = 0.975, na.rm = TRUE),
    .by = c(prior, percent, other)
  ) %>%
  mutate(
    percent = factor(x = percent(percent), levels = percent(pct_list), ordered = TRUE)
  )

# figure S6: change in relative volume sales across different cutoffs
p1 <- post_info %>%
  filter(other > dates[T0]) %>%
  ggplot(mapping = aes(x = other, y = mean, ymin = lwr_CI, ymax = upp_CI, color = percent)) +
  geom_errorbar() +
  geom_line() +
  geom_point(size = 1.75) +
  scale_color_discrete_sequential(
    name = "",
    palette = "Blues 2",
    nmax = 11
  ) +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(labels = comma) +
  labs(
    x = TeX("Aggregated Four-Week Sales Reporting Period"),
    y = TeX("Change in Relative Volume Sales in Ounces, $\\hat{\\tau}_t$")
  ) +
  theme_bw(base_size = 12) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 12, 0, 0)),
    legend.position = "right",
    panel.spacing = unit(5, "mm"),
    strip.background =element_rect(fill = "grey90")
  )

save_plot("sfigs/sfig06.pdf", plot = p1, width = 8, height = 8)
