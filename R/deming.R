#' Bayesian linear regression with Stan
#'
#' @param df Nested tibble, output from `sim_data`
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
#'
deming <- function(df, priors, formula = y ~ x, z = NULL, power = NULL, theta_range = NULL, init = NULL, chains = 4, ...) {

  standata <- parse_df(df, formula, z)
  standata <- add_priors(standata, priors, power, theta_range)

  mod <- get_deming_model(standata, priors, power, theta_range)

  if (is.null(init)) {
    x_means <- vapply(df$x, mean, numeric(1))
    init <- rep(list(list(theta = x_means)), length.out = chains)
  }

  rstan::sampling(mod, data = standata, init = init, ...)

}

# Parse relevant args of deming() to determine appropriate stan model
get_deming_model <- function(standata, priors, power, theta_range) {

  # power prior model or not?
  if (is.null(power)) {

    stopifnot("Extra data provided (z), but power prior model not specified" = ! "z" %in% names(standata))

    # prior on thetas flat or uniform?
    if (!is.null(theta_range)) {
      mod <- stanmodels$deming_uniform
    } else {
      mod <- stanmodels$deming_flat
    }

  } else {

    if (power == "point") {
      if (is.null(theta_range)) {
        mod <- stanmodels$deming_flat_power_point
      } else {
        mod <- stanmodels$deming_uniform_power_point
      }
    } else if (power == "beta" | power == "unif") {
      if (is.null(theta_range)) {
        mod <- stanmodels$deming_flat_power_beta
      } else {
        mod <- stanmodels$deming_uniform_power_beta
      }
    } else if (power == "normal") {
      if (is.null(theta_range)) {
        mod <- stanmodels$deming_flat_power_normal
      } else {
        mod <- stanmodels$deming_uniform_power_normal
      }
    }

  }

  mod
}
