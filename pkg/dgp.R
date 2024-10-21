# """
# Functions for the data generating process used in the numerical experiments.
# """

#'
#' Compute the observed outcomes under the intervention
#'
compute_observed_outcomes <- function(simout, tau, xi, sp_pct)
{
  X  <- simout[["X"]]
  d  <- simout[["d"]]
  Y0 <- simout[["Y0"]]

  n <- nrow(Y0)
  p <- ncol(Y0)

  rho_0 <- quantile(d, p = sp_pct, names = FALSE)

  Xi <- matrix(data = xi * exp(-d) * (d < rho_0), nrow = n, ncol = p, byrow = TRUE)

  V <- rep(0, times = n)
  A <- rep(0, times = p)

  V[n] <- 1L
  A[1] <- 1L

  Y_obs <- Y0 + tau * tcrossprod(V, A) + Xi * tcrossprod(V, 1 - A)

  return(Y_obs)
}

#'
#' Generate the potential outcomes in the absence of the intervention
#'
simulate_counterfactuals <- function(
  T_0, J, theta,
  K = 4,
  mu_Phi = c(1, 0, 1, 0), sigma_Phi = c(0.5, 0.5, 0.5, 0.5),
  a_M = c(1, 0, 0, 0), b_M = c(1, 1, 1, 1),
  sigma_eps = 1,
  sigma_d = 1, mu_d = 0
)
{
  n <- T_0 + 1
  p <- J + 1
  q <- length(theta)

  Phi <- factor_loadings(n, K, mu = mu_Phi, sigma = sigma_Phi)
  M   <- common_factors(p, K, min = a_M, max = b_M)
  E   <- transitory_shocks(n, p, sigma = sigma_eps)

  Theta <- matrix(theta, nrow = n, ncol = q, byrow = TRUE)

  X <- mvtnorm::rmvnorm(p, sigma = diag(q))
  d <- truncnorm::rtruncnorm(p, a = 0, b = Inf, mean = 0, sd = sigma_d)

  d[1] <- 0.0

  # distributional differences between neighboring and non-neighboring controls
  if (mu_d != 0)
  {
    outside <- rbinom(p, size = 1, prob = d / max(d))
    shift   <- matrix(data = mu_d * outside, nrow = p, ncol = q, byrow = FALSE)
    X       <- X + shift
  }

  Y0 <- tcrossprod(Theta, X) + tcrossprod(Phi, M) + E

  return(list(X = X, d = d, Y0 = Y0))
}

#'
#' Uniform distributed common factors
#'
common_factors <- function(n, K, min, max)
{
  x <- runif(n * K, min = min, max = max)
  return(matrix(x, nrow = n, ncol = K, byrow = TRUE))
}

#'
#' Generate factor loadings with an first-order autoregressive process
#'
factor_loadings <- function(n, K, mu, sigma)
{
  Phi <- mvtnorm::rmvnorm(n, sigma = diag(K))

  for (t in seq_len(n - 1))
  {
    Phi[t + 1, ] <- Phi[t + 1, ] + mu + sigma * Phi[t, ]
  }

  return(Phi)
}

#'
#' Gaussian distributed transitory shocks
#'
transitory_shocks <- function(n, p, sigma)
{
  x <- rnorm(n * p, mean = 0, sd = sigma)
  return(matrix(x, nrow = n, ncol = p))
}

