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
  real theta_min;
  real theta_max;

}

parameters {
  real alpha;
  real beta;
  vector<lower=theta_min, upper=theta_max>[K] theta;

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
  int pos;

  // Priors
  alpha ~ normal(alpha_mean, alpha_sd);
  beta ~ normal(beta_mean, beta_sd);

  // Likelihood for x values
  pos = 1;
  for (k in 1:K) {
    segment(X, pos, Jx[k]) ~ normal(theta[k], sigma_x);
    pos = pos + Jx[k];
  }

  // Likelihood for y values
  pos = 1;
  for (k in 1:K) {
    segment(Y, pos, Jy[k]) ~ normal(nu[k], sigma_y);
    pos = pos + Jy[k];
  }

}
