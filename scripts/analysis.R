# """
# This script estimates the causal effect of Philadelphia's beverage tax on
# sales of sugar-sweetened and artificially sweetened beverages at
# mass merchandise stores in the city.
#
# The estimates are obtained using the distance-based shrinkage priors
# with weighted distances under various importance weights.
# """

source("pkg/models.R")
source("pkg/ops.R")
source("pkg/private.R")
source("pkg/utilities.R")

load("data/sample_information.RData")

set.seed(123, kind = "L'Ecuyer-CMRG")

cutoff_pct <- 0.25
n_chains   <- 4
n_burn     <- 5000
n_iter     <- 5000
kappa_list <- c(0.0, 0.1, 0.5, 1.0)

if (!exists("beverage_sales"))
{
  stop("FileNotFound: R Object 'beverage_sales' must be loaded.")
}

std <- standardize_matrix(beverage_sales, train = seq_len(T0))
X   <- scale(baseline_covariates)

Y <- std[["x"]]

draws_out <- data.frame()

for (kappa_d in kappa_list)
{
  d_C <- weighted_distance(X, d, kappa_d = kappa_d)
  rho <- quantile(d_C, p = cutoff_pct, names = FALSE)

  tau_DHS <- db_bayesian_sc(
    Y, T0, d = d_C, rho = rho,
    prior = "DHS",
    refresh = 1000,
    chains = n_chains,
    parallel_chains = 4,
    iter_warmup = n_burn, iter_sampling = n_iter,
    max_treedepth = 12, adapt_delta = 0.99
  )

  tau_DS2 <- db_bayesian_sc(
    Y, T0, d = d_C, rho = rho,
    prior = "DS2",
    refresh = 1000,
    chains = n_chains,
    parallel_chains = 4,
    iter_warmup = n_burn, iter_sampling = n_iter,
    max_treedepth = 12, adapt_delta = 0.99,
    force_recompile = TRUE
  )

  tau_DHS <- std$s_hat[1] * tau_DHS
  tau_DS2 <- std$s_hat[1] * tau_DS2

  dhstab <- multiple_chains_to_dataframe(
    tau_DHS,
    other_info = data.frame(prior = "DHS", kappa_d = kappa_d),
    pivot = TRUE,
    extra_ids = dates,
    cols = starts_with("tau"),
    names_to = "estimator",
    values_to = "draw"
  )

  ds2tab <- multiple_chains_to_dataframe(
    tau_DS2,
    other_info = data.frame(prior = "DS2", kappa_d = kappa_d),
    pivot = TRUE,
    extra_ids = dates,
    cols = starts_with("tau"),
    names_to = "estimator",
    values_to = "draw"
  )

  draws_out <- rbind(draws_out, dhstab, ds2tab)
}

saveRDS(draws_out, file = "output/beverage_tax_causal_effect_estimates.RDS")
