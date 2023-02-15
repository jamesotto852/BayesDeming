#' Bayesian linear regression with Stan
#'
#' @param df Nested tibble, output from `sim_data`
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
#'
deming <- function(df, priors, formula = y ~ x, z = NULL, power = NULL, empirical = FALSE, ...) {

  standata <- parse_df(df, formula, z)
  standata <- add_priors(standata, priors, power)

  mod <- get_deming_model(standata, priors, power, empirical = empirical)

  rstan::sampling(mod, data = standata, ...)

}

get_deming_model <- function(standata, priors, power, empirical) {
  if (is.null(power)) {
    stopifnot("Extra data provided (z), but power prior model not specified" = ! "z" %in% names(standata))
    if (empirical) {
      stanmodels$deming_empirical
    } else {
      stanmodels$deming
    }
  } else if (power == "point") {
    stanmodels$deming_power
  } else if (power == "beta" | power == "unif") {
    stanmodels$deming_power_beta
  } else if (power == "normal") {
    stanmodels$deming_power_normal
  }
}
