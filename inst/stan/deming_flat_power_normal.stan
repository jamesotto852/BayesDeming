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
  real<lower=0> sigma_x_min;
  real<lower=0> sigma_y_min;
  real<lower=0> sigma_x_max;
  real<lower=0> sigma_y_max;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real alpha_mean;
  real beta_mean;
  real<lower=0, upper=1> power_mean;
  real<lower=0> power_sd;

  // Prior data:
  int<lower=0> Nz;
  vector[Nz] z;
}

transformed data {
  vector[Nz] z_demeaned;
  z_demeaned = z - mean(z);
}

parameters {
  real alpha;
  real beta;
  vector[K] theta;

  real<lower=sigma_x_min, upper=sigma_x_max> sigma_x;
  real<lower=sigma_y_min, upper=sigma_y_max> sigma_y;

  real<lower=0, upper=1> power;
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

  // Increment according to power priors
  // Do we lose a degree of freedom?
  power ~ normal(power_mean, power_sd);
  target += power * normal_lpdf(z_demeaned| 0, sigma_x);

}









