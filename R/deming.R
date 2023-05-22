#' Bayesian linear regression with Stan
#'
#' @inheritParams rstan::sampling
#'
#' @param df either a `data.frame` with columns `value`, `var`, and `grp`; or a tibble with list columns. See "data structure" section below for details.
#' @param priors a named list setting values for prior hyperparameters. See "priors" section below for details.
#' @param formula an object of class `"formula"`. In model output, the LHS (RHS) is referred to as "y" ("x").
#' @param x0 an optional vector representing an additional group of X values observed without a corresponding Y group.
#' @param power must be specified if `x0` is included, the power prior to use for prior data.
#' Options are `"point"`, `"beta"`, `"normal"`, and `"unif"`.
#' Additional prior parameters may need to specified via `priors` argument.
#' @param theta_range Range to use for uniform prior on thetas (group means). If unspecified an improper flat prior is used; this is recommended.
#' @param ... arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#' @section data structure:

#' The `df` argument must be a `data.frame`.
#' As the data used in these models does not consist of ordered pairs
#' the structure of this value is slightly complicated.
#'
#' `df` can be supplied as a `data.frame` with three columns: `value`, `var`, and `grp`.
#' In this case, `value` is the value of both X and Y observations,
#' `var` indicates whether a row corresponds to an X or Y observation,
#' and `grp` labels a row as belonging to a group i = 1, ..., I.
#'
#' Alternatively, `df` can be supplied as a `tibble` with list columns.
#' These must have elements that are numeric vectors,
#' corresponding to groups of repeated measures.
#'
#' In both cases, the `formula` argument must be specified.
#' If a `data.frame` is specified, the formula relates the values in the `var` column.
#' If a `tibble` with list columns is specified, the formula relates the relevant column names.
#' See the below "examples" section for an illustration of both methods of data specification.
#'
#' @section priors:

#' The `priors` argument is a list that is necessary to specify values
#' governing the prior distributions used by the model.
#'
#' To specify the priors on alpha and beta, the intercept and slope relating X and Y values,
#' `alpha_mean`, `alpha_sd`, `beta_mean`, and `beta_sd` must be specified.
#' These define the normal priors placed on these parameters.
#'
#' To define the priors on the variance components sigma_x and sigma_y
#' it is necessary to specify values `sigma_x_max` and `sigma_y_max`.
#' These are used as the bounds for the uniform priors on these parameters.
#' It is also possible to specify `sigma_x_min` and `sigma_y_min`
#' as lower bounds for these distributions.
#'
#' It is possible that additional values need to be specified if prior data is provided via the `x0` argument.
#' If `power` is `"point"`, `power` must be specified; this is the value the power prior is fixed at.
#' If `power` is `"beta"`, `power_a` and `power_b` must be specified
#' Similarly, if `power` is `"normal"`, `power_mean` and `power_sd` must be specified.
#' These correspond to the mean and standard deviation of the normal distribution that is truncated to [0, 1].
#'
#' @examples
#'
#' df <- sim_data(
#'   Jx = c(6, 6, 6, 6, 6, 6, 6, 6),
#'   Jy = c(4, 5, 5, 4, 5, 5, 5, 7),
#'   theta = 1:8,
#'   sdx = 1, sdy = 1,
#'   alpha = 0, beta = 1
#' )
#'
#' priors <- list(
#'   sigma_x_max = 10,
#'   sigma_y_max = 10,
#'   alpha_mean = 0,
#'   alpha_sd = 10,
#'   beta_mean = 0,
#'   beta_sd = 10
#' )
#'
#' res <- deming(df, priors, y ~ x)
#'
#' # can also use `deming()` without list columns:
#' df_long <- dplyr::mutate(df, grp = 1:8)
#' df_long <- tidyr::pivot_longer(df_long, c(x, y), names_to = "var")
#' df_long <- tidyr::unnest(df_long, value)
#'
#' res <- deming(df, priors, y ~ x)
#'
#' @export
#'
deming <- function(df, priors, formula = y ~ x, x0 = NULL, power = NULL, theta_range = NULL, init = NULL, chains = 4, ...) {

  standata <- parse_df(df, formula, x0)
  standata <- add_priors(standata, priors, power, theta_range)

  mod <- get_deming_model(standata, priors, power, theta_range)

  if (is.null(init)) {
    x <- df[deparse(formula[[3]])][[1]]
    x_means <- vapply(x, mean, numeric(1))
    init <- rep(list(list(theta = x_means)), length.out = chains)
  }

  rstan::sampling(mod, data = standata, init = init, ...)

}

# Parse relevant args of deming() to determine appropriate stan model
get_deming_model <- function(standata, priors, power, theta_range) {

  # power prior model or not?
  if (is.null(power)) {

    stopifnot("Extra data provided (x0), but power prior model not specified" = ! "z" %in% names(standata))

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
