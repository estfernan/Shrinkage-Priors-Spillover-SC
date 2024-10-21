# """
# Utility functions used within the project scripts.
# """

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
#' Save and crop a ggplot object
#'
save_plot <- function(filename, plot = ggplot2::last_plot(), create.dir = TRUE, quiet = TRUE, ...)
{
  ggplot2::ggsave(filename, plot = plot, create.dir = create.dir, ...)
  knitr::plot_crop(filename, quiet = quiet)
  return(invisible(x = filename))
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
