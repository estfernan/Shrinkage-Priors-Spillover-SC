# """
# Utility functions used within the project scripts.
# """

#'
#' Form Column Standard Deviations
#'
colSDs <- function(x, na.rm = FALSE)
{
  apply(x, 2, sd, na.rm = na.rm)
}

#'
#' Combine the results of an individual replication
#'
combine_replication <- function(lst, iter, T_0, J, kappa_d, mu_d, spillover_percentages)
{
  nms <- names(lst)
  N   <- length(lst)

  repout <- vector(mode = "list", length = N)

  for (v in seq_along(lst))
  {
    repout[[v]] <- single_replication(
      lst[[v]], nms[[v]],
      iter,
      T_0, J,
      kappa_d, mu_d,
      spillover_percentages
    )
  }

  return(do.call("rbind", repout))
}

#'
#' Compute the finite-sample properties from the set of numerical experiments
#'
empirical_results <- function(indir = "Results/Replications")
{
  pckg_loaded <- "dplyr" %in% .packages()

  if (!pckg_loaded)
  {
    stop("PackageNotFound: 'dplyr' must be loaded before running this function.")
  }

  simtab <- list.files(indir, pattern = "RDS", full.names = TRUE) %>%
    lapply(readRDS) %>%
    do.call("rbind", .) %>%
    group_by(name, T_0, J, kappa_d, mu_d, spillover) %>%
    mutate(n_reps = n(), .after = spillover) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    ungroup() %>%
    select(-iter)

  return(simtab)
}

#'
#' Convert multiple chains into a data.frame object
#'
multiple_chains_to_dataframe <- function(x, other_info = NULL, pivot = FALSE, extra_ids = NULL, ...)
{
  n_chains <- posterior::nchains(x)
  n_iter   <- posterior::ndraws(x) / n_chains

  lst <- lapply(
    seq_len(n_chains),
    single_chain_to_dataframe,
    x = x,
    other_info = other_info
  )

  tabout <- do.call("rbind", lst)

  if (pivot)
  {
    pckg_loaded <- "dplyr" %in% .packages()

    if (!pckg_loaded)
    {
      stop("PackageNotFound: 'dplyr' must be loaded before running this function.")
    }

    tabout <- tabout %>%
      pivot_longer(...) %>%
      mutate(other = rep(extra_ids, times = n_iter * n_chains), .after = iter)
  }

  return(tabout)
}

#'
#' Save and crop a ggplot object
#'
save_plot <- function(filename, plot = ggplot2::last_plot(), create.dir = TRUE, quiet = TRUE, ...)
{
  ggplot2::ggsave(filename, plot = plot, create.dir = create.dir, ...)
  knitr::plot_crop(filename, quiet = quiet)
  return(invisible(x = filename))
}

#'
#' Convert a single chain into a data.frame object
#'
single_chain_to_dataframe <- function(x, chain = 1, other_info = NULL)
{
  tmp_draws <- posterior::subset_draws(x, chain = chain)

  tabout <- data.frame(
    chain = chain,
    iter = 1:nrow(tmp_draws),
    tmp_draws,
    check.names = FALSE
  )

  if (!is.null(other_info))
  {
    tabout <- cbind(other_info, tabout)
  }

  return(tabout)
}

#'
#' Scaling and Centering of Matrix-like Objects
#'
standardize_matrix <- function(x, train = NULL)
{
  tmp <- x

  if (!is.null(train))
  {
    x <- x[train, ]
  }

  x_bar <- colMeans(x)
  s_hat <- colSDs(x)

  x_cent <- sweep(sweep(tmp, 2, x_bar, FUN = "-"), 2, s_hat, FUN = "/")

  return(list(x = x_cent, x_bar = x_bar, s_hat = s_hat))
}

#'
#' Vector to RGB conversion
#'
vec2rgb <- function(x, alpha, names = NULL, maxColorValue = 1)
{
  return(rgb(x[1], x[2], x[3], alpha, names = names, maxColorValue = maxColorValue))
}

#'
#' Conservative 7-color palette
#'
wong_colors <- function(alpha = 1.0)
{
  colors <- list(
    c(0/255, 114/255, 178/255), # blue
    c(230/255, 159/255, 0/255), # orange
    c(0/255, 158/255, 115/255), # green
    c(204/255, 121/255, 167/255), # reddish purple
    c(86/255, 180/255, 233/255), # sky blue
    c(213/255, 94/255, 0/255), # vermilion
    c(240/255, 228/255, 66/255) # yellow
  )

  return(sapply(colors, vec2rgb, alpha = alpha))
}
