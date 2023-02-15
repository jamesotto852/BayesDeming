data {
  // # observations
  int<lower=0> Nx;
  int<lower=0> Ny;

  // # of groups
  int<lower=0> K;

  // group sizes
  int Jx[K];
  int Jy[K];

  // observations
  vector[Nx] X;
  vector[Ny] Y;

  // hyperpriors
  real<lower=0> sigma_x_max;
  real<lower=0> sigma_y_max;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real alpha_mean;
  real beta_mean;

}

transformed data {
  vector[K] theta;

  // Declare indexer for ragged array traversal
  int pos_transform;

  // Likelihood for x values
  pos_transform = 1;
  for (k in 1:K) {
    theta[k] = mean(segment(X, pos_transform, Jx[k]));
    pos_transform = pos_transform + Jx[k];
  }

}

parameters {
  real alpha;
  real beta;

  real<lower=0, upper=sigma_x_max> sigma_x;
  real<lower=0, upper=sigma_y_max> sigma_y;
}

transformed parameters {
  real lambda;
  vector[K] nu;

  nu = alpha + beta * theta;
  lambda = (sigma_y / sigma_x)^2;
}

model {
  // Declare indexer for ragged array traversal
  int pos_model;

  // Priors
  alpha ~ normal(alpha_mean, alpha_sd);
  beta ~ normal(beta_mean, beta_sd);

  // Likelihood for x values
  pos_model = 1;
  for (k in 1:K) {
    segment(X, pos_model, Jx[k]) ~ normal(theta[k], sigma_x);
    pos_model = pos_model + Jx[k];
  }

  // Likelihood for y values
  pos_model = 1;
  for (k in 1:K) {
    segment(Y, pos_model, Jy[k]) ~ normal(nu[k], sigma_y);
    pos_model = pos_model + Jy[k];
  }

}
