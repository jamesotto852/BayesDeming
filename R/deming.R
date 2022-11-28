#' Bayesian linear regression with Stan
#'
#' @param df Nested tibble, output from `sim_data`
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @export
#'
deming <- function(df, priors, formula = y ~ x, z = NULL, ...) {

  standata <- parse_df(df, formula, z)
  standata <- add_priors(standata, priors)

  mod <- get_deming_model(standata, priors)

  rstan::sampling(mod, data = standata, ...)

}

get_deming_model <- function(standata, priors) {
  if (! "z" %in% names(standata)) {
    # message("deming")
    stanmodels$deming
  } else if ("power" %in% names(priors)) {
    # message("deming_power")
    stanmodels$deming_power
  } else if ("power_a" %in% names(priors)) {
    # message("deming_power_hyper_prior")
    stanmodels$deming_power_hyper_prior
  }
}
