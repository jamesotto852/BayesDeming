test_that("model assignment logic works", {

  check_stan_model <- function(power, theta_range, mod_expected) {
    expect_identical(
      BayesDeming:::get_deming_model(standata = NULL, priors = NULL, power = power, theta_range = theta_range),
      mod_expected
    )
  }

  # Models without power priors:
  check_stan_model(power = NULL, theta_range = NULL,     BayesDeming:::stanmodels$deming_flat)
  check_stan_model(power = NULL, theta_range = c(0, 10), BayesDeming:::stanmodels$deming_uniform)

  # Power prior models:
  check_stan_model(power = "point",  theta_range = NULL, BayesDeming:::stanmodels$deming_flat_power_point)
  check_stan_model(power = "beta",   theta_range = NULL, BayesDeming:::stanmodels$deming_flat_power_beta)
  check_stan_model(power = "unif",   theta_range = NULL, BayesDeming:::stanmodels$deming_flat_power_beta) # unif is in beta family
  check_stan_model(power = "normal", theta_range = NULL, BayesDeming:::stanmodels$deming_flat_power_normal)

  check_stan_model(power = "point",  theta_range = c(0, 10), BayesDeming:::stanmodels$deming_uniform_power_point)
  check_stan_model(power = "beta",   theta_range = c(0, 10), BayesDeming:::stanmodels$deming_uniform_power_beta)
  check_stan_model(power = "unif",   theta_range = c(0, 10), BayesDeming:::stanmodels$deming_uniform_power_beta) # unif is in beta family
  check_stan_model(power = "normal", theta_range = c(0, 10), BayesDeming:::stanmodels$deming_uniform_power_normal)

})
