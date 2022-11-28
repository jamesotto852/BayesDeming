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
  real<lower=0> alpha_var;
  real<lower=0> beta_var;
  real alpha_mean;
  real beta_mean;

}

parameters {
  real alpha;
  real beta;
  vector[K] theta;

  real<lower=0, upper=sigma_x_max> sigma_x;
  real<lower=0, upper=sigma_y_max> sigma_y;
}

transformed parameters {
  vector[K] nu;
  nu = alpha + beta * theta;
}

model {
  // Declare indexer for ragged array traversal
  int pos;

  // Priors
  alpha ~ normal(alpha_mean, alpha_var);
  beta ~ normal(beta_mean, beta_var);

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
