# """
# This script runs a set of numerical experiments to evaluate the finite-sample
# properties of the Bayesian SC with distance-based shrinkage priors,
# alongside two comparison methods -- the Bayesian structural time-series (BSTS)
# model and the generalized synthetic control (GSC) method.
#
# Our setting focuses on estimating the treatment effect in the presence of
# distance-based spillover effects, where there may be distributional differences
# in the covariates for neighboring and non-neighboring controls.
# """

source("pkg/dgp.R")
source("pkg/models.R")
source("pkg/ops.R")
source("pkg/private.R")
source("pkg/srun.R")
source("pkg/utilities.R")

load("data/random_seeds.RData")

i <- strtoi(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1L))

set.seed(random_seeds[i], kind = "L'Ecuyer-CMRG")

# set up data generating settings and parameters
T_0      <- 30
J        <- 50
kappa_d  <- 0
mu_d     <- 0
tau      <- 7
xi       <- -10
n_burn   <- 5000
n_iter   <- 5000
sp_pct   <- seq(from = 0, to = 0.5, by = 0.05)
cli_args <- commandArgs(trailingOnly = TRUE)

# extract command line arguments - if available
if (length(cli_args) > 0)
{
  T_0     <- strtoi(cli_args[1])
  J       <- strtoi(cli_args[2])
  kappa_d <- as.numeric(cli_args[3])
  mu_d    <- as.numeric(cli_args[4])
}

datlist <- simulate_counterfactuals(
  T_0, J,
  theta = c(-0.5, 0.5),
  K = 4, mu_Phi = c(1, 0, 1, 0), sigma_Phi = c(0.5, 0.5, 0.5, 0.5),
  a_M = c(1, 0, 0, 0), b_M = c(1, 1, 1, 1),
  sigma_eps = 1,
  mu_d = mu_d
)

d_C <- weighted_distance(datlist$X, datlist$d, kappa_d = kappa_d)
rho <- quantile(d_C, p = 0.25, names = FALSE)

BSTS <- srunner_bsts(
  sp_pct, datlist,
  T_0, tau, xi,
  n_burn = n_burn, n_iter = n_iter,
  prior.level.sd = 0.01
)

dhs <- srunner_dbsc(
  sp_pct, datlist,
  d_C, rho,
  T_0,
  tau, xi,
  prior = "DHS",
  refresh = 0,
  chains = 1,
  iter_warmup = n_burn, iter_sampling = n_iter,
  max_treedepth = 12, adapt_delta = 0.99
)

ds2 <- srunner_dbsc(
  sp_pct, datlist,
  d_C, rho,
  T_0,
  tau, xi,
  prior = "DS2",
  refresh = 0,
  chains = 1,
  iter_warmup = n_burn, iter_sampling = n_iter,
  max_treedepth = 12, adapt_delta = 0.99
)

gsc <- srunner_gsc(
  sp_pct, datlist,
  T_0,
  tau, xi,
  n_boot = 1000,
  force = "two-way", inference = "parametric",
  parallel = FALSE
)

# export results into a single summary table
simout <- combine_replication(
  list("BSTS" = BSTS, "DHS" = DHS, "DS2" = DS2, "GSC" = GSC),
  i, T_0, J,
  kappa_d, mu_d, sp_pct
)

outfile <- sprintf("%04d_Replication_%02d-%02d-%03d-%03d.RDS", i, T_0, J, 100 * kappa_d, 10 * mu_d)
outpath <- file.path("output/reps", outfile)
saveRDS(simout, file = outpath)
