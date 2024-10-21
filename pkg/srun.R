# """
# Functions to run individual models within the numerical experiments.
# """

#'
#' Bayesian structural time-series model
#'
srunner_bsts <- function(sp_pct, datlist, T_0, tau, xi, ...)
{
  bsts_applied <- function(pct, datlist, T_0, tau, xi, ...)
  {
    Y_obs   <- compute_observed_outcomes(datlist, tau = tau, xi = xi, sp_pct = pct)
    tau_hat <- causal_impact(Y_obs, T_0, ...)
    return(bayes_estimator(tau_hat[, T_0 + 1], tau))
  }

  simout <- lapply(
    sp_pct, bsts_applied,
    datlist = datlist,
    T_0 = T_0,
    tau = tau, xi = xi,
    ...
  )

  return(simout)
}

#'
#' Synthetic control with distance-based shrinkage priors
#'
srunner_dbsc <- function(sp_pct, datlist, d_C, rho, T_0, tau, xi, prior, ...)
{
  dbsc_applied <- function(pct, datlist, d_C, rho, T_0, tau, xi, prior, ...)
  {
    Y_obs   <- compute_observed_outcomes(datlist, tau = tau, xi = xi, sp_pct = pct)
    tau_hat <- db_bayesian_sc(Y_obs, T_0, d = d_C, rho = rho, prior = prior, ...)
    return(bayes_estimator(tau_hat[, T_0 + 1], tau))
  }

  simout <- lapply(
    sp_pct, dbsc_applied,
    datlist = datlist,
    d_C = d_C, rho = rho,
    T_0 = T_0,
    tau = tau, xi = xi,
    prior = prior,
    ...
  )

  return(simout)
}

#'
#' Generalized synthetic control method
#'
srunner_gsc <- function(sp_pct, datlist, T_0, tau, xi, ...)
{
  gsc_applied <- function(pct, datlist, T_0, tau, xi, ...)
  {
    Y_obs   <- compute_observed_outcomes(datlist, tau = tau, xi = xi, sp_pct = pct)
    tau_hat <- gsc_method(Y_obs, T_0, ...)
    return(bayes_estimator(tau_hat[, T_0 + 1], tau))
  }

  simout <- lapply(
    sp_pct, gsc_applied,
    datlist = datlist,
    T_0 = T_0,
    tau = tau, xi = xi,
    ...
  )

  return(simout)
}
