# """
# Statistical models used within the project.
# """

#'
#' Bayesian structural time-series model
#'
causal_impact <- function(Y, T_0, n_burn = 3000, n_iter = 2000, ...)
{
  n <- nrow(Y)
  p <- ncol(Y)

  J <- p - 1

  Y_tr <- Y[seq_len(T_0), 1]
  X_tr <- Y[seq_len(T_0), -1]

  Y_obs <- Y[, 1]
  X_obs <- Y[, -1]

  pre_T0  <- c(1, T_0)
  post_T0 <- c(T_0 + 1, n)

  args <- list(niter = n_burn + n_iter, ...)

  fit <- CausalImpact::CausalImpact(
    data = Y,
    pre.period = pre_T0, post.period = post_T0,
    model.args = args
  )

  Y0_hat   <- fit$model$posterior.samples
  tau_hat  <- -sweep(Y0_hat, 2, Y_obs)
  beta_hat <- fit$model$bsts.model$coefficients

  burn <- seq_len(n_burn)

  Y0_hat   <- Y0_hat[-burn, ]
  tau_hat  <- tau_hat[-burn, ]
  beta_hat <- beta_hat[-burn, ]

  return(tau_hat)
}

#'
#' Bayesian SC method with distance-based shrinkage priors
#'
db_bayesian_sc <- function(
    Y, T_0,
    d = 1, rho = 0,
    prior = "DHS",
    ...,
    stan_dir = "pkg/stan",
    force_recompile = FALSE
)
{
  filename <- switch(prior, "DHS" = "SC-DHS", "DS2" = "SC-DS2")

  stanmodel <- init_cmdstan_model(filename, stan_dir, force_recompile = force_recompile)

  n <- nrow(Y)
  p <- ncol(Y)

  J <- p - 1

  Y_tr <- Y[seq_len(T_0), 1]
  X_tr <- Y[seq_len(T_0), -1]

  Y_obs <- Y[, 1]
  X_obs <- Y[, -1]

  if (length(d) == 1)
  {
    d <- rep(d, times = J)
  }

  standat <- list(
    J = J, T_0 = T_0,
    K = n,
    Y_tr = Y_tr, X_tr = X_tr,
    Y_obs = Y_obs, X_obs = X_obs,
    d = d
  )

  if (prior == "DS2")
  {
    standat <- c(standat, rho = rho)
  }

  fit <- stanmodel$sample(data = standat, ...)

  Y0_hat  <- fit$draws(variables = "Y", format = "matrix")
  tau_hat <- fit$draws(variables = "tau", format = "matrix")

  return(tau_hat)
}

#'
#' Generalized synthetic control method
#'
gsc_method <- function(Y, T_0, n_boot = 1000, ...)
{
  dtab <- sc_longdata(Y, T_0)

  fit <- gsynth::gsynth(
    formula = Y ~ D, data = dtab,
    index = c("unit","time"),
    se = TRUE,
    nboots = n_boot,
    ...
  )

  Y0_hat   <- fit$Y.ct
  tau_hat  <- t(fit$att.boot)
  beta_hat <- fit$wgt.implied

  return(tau_hat)
}
