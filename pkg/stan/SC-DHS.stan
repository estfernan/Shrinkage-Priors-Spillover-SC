/*
 * Bayesian synthetic control method with a distance-based horseshoe prior
 */

data
{
  int<lower=1> J;                 // number of control units
  int<lower=1> T_0;               // number of pre-intervention periods
  int<lower=1> K;                 // number of periods to forecast
  vector[T_0] Y_tr;               // pre-intervention outcomes for the treated unit
  matrix[T_0, J] X_tr;            // pre-intervention outcomes for the control units
  vector[K] Y_obs;                // observed outcomes for the treated unit
  matrix[K, J] X_obs;             // predictor outcomes for the control units
  vector<lower=0, upper=1>[J] d;  // weighted distances for the control units
}

parameters
{
  real psi;                   // coefficient for the lagged outcome term
  vector[J] beta;             // coefficients for the synthetic control
  vector<lower=0>[J] lambda;  // local shrinkage parameter
  real<lower=0> zeta;         // global shrinkage parameter
  real<lower=0> sigma;        // standard deviation for the outcomes
}

model
{
  psi ~ normal(0, 3);
  beta ~ normal(0, lambda * zeta);
  lambda ~ cauchy(0, d);
  zeta ~ cauchy(0, sigma^2);
  sigma ~ student_t(4, 0, 1);

  for (t in 1:T_0)
  {
    real lag_term = (t == 1) ? 0.0 : psi * Y_tr[t - 1];
    real mu = lag_term + X_tr[t, ] * beta;

    Y_tr[t] ~ normal(mu, sigma);
  }
}

generated quantities
{
  vector[K] Y;
  vector[K] tau;

  for (t in 1:K)
  {
    real lag_term = (t == 1) ? 0.0 : psi * Y[t - 1];
    real mu = lag_term + X_obs[t, ] * beta;

    Y[t] = normal_rng(mu, sigma);
    tau[t] = Y_obs[t] - Y[t];
  }
}
