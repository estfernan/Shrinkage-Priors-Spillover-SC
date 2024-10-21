# """
# Functions for mathematical and statistical operations.
# """

#'
#' Finite-sample properties of a Bayesian estimator
#'
bayes_estimator <- function(draws, truth, alpha = 0.05)
{
  upp_prob <- 1 - alpha / 2
  lwr_prob <- 1 - upp_prob

  post_mean <- mean(draws, na.rm = TRUE)
  post_se   <- sd(draws, na.rm = TRUE)
  lwr_CI    <- quantile(draws, p = lwr_prob, na.rm = TRUE, names = FALSE)
  upp_CI    <- quantile(draws, p = upp_prob, na.rm = TRUE, names = FALSE)
  bias      <- post_mean - truth
  pct_bias  <- bias / abs(truth)
  coverage  <- lwr_CI < truth & truth < upp_CI
  width     <- upp_CI - lwr_CI

  props <- c(
    truth = truth,
    mean = post_mean, SE = post_se,
    lwr_CI = lwr_CI, upp_CI = upp_CI,
    bias = bias,
    pct_bias = pct_bias,
    RMSE = sqrt(bias^2 + post_se^2),
    coverage = coverage, CI_width = width
  )

  return(props)
}

#'
#' Weighted distance function
#'
weighted_distance <- function(X, d, kappa_d = 0, i = 1, S = max(d, na.rm = TRUE), exclude_treated = TRUE)
{
  d_X <- 1 / (1 + apply(sweep(X, 2, X[i, ], FUN = "-"), 1, norm, type = "2"))
  d_P <- d / S

  d_C <- kappa_d * d_X + (1 - kappa_d) * d_P

  if (exclude_treated)
  {
    d_C <- d_C[-i]
  }

  return(d_C)
}
