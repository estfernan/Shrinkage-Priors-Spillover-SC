# """
# Private functions not use in regular scripts.
# """

#'
#' Create a new CmdStanModel object
#'
init_cmdstan_model <- function(filename, stan_path, ..., parent_dir = getwd())
{
  path <- file.path(parent_dir, stan_path)

  exe_file  <- file.path(path, filename)
  stan_file <- paste0(exe_file, ".stan")

  stanmodel <- cmdstanr::cmdstan_model(stan_file = stan_file, exe_file = exe_file, ...)

  return(stanmodel)
}

#'
#' Convert synthetic control matrix to a data frame object
#'
sc_longdata <- function(Y, T_0)
{
  n <- nrow(Y)
  p <- ncol(Y)

  V <- rep(0, times = n)
  A <- rep(0, times = p)

  post_T0 <- seq(from = T_0 + 1, to = n, by = 1)

  V[post_T0] <- 1L
  A[1]       <- 1L

  dtab <- data.frame(
    reshape2::melt(Y, varnames = c("time", "unit"), value.name = "Y"),
    D = rep(A, each = n) * rep(V, times = p)
  )

  return(dtab)
}

#'
#' Process the results of an individual replication for one model type
#'
single_replication <- function(x, name, iter, T_0, J, kappa_d, mu_d, spillover_percentages)
{
  rtab <- data.frame(
    iter = iter,
    name = name,
    T_0 = T_0, J = J,
    kappa_d = kappa_d,
    mu_d = mu_d,
    spillover = spillover_percentages,
    do.call("rbind", x)
  )

  return(rtab)
}
