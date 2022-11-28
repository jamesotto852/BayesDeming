# Helper function, convert nested df into data list for stan
parse_df <- function(df, formula, z) {

  # Formula should be simple to parse
  # (add functionality later)
  x <- df[deparse(formula[[3]])][[1]]
  y <- df[deparse(formula[[2]])][[1]]

  x_vec <- unlist(x)
  y_vec <- unlist(y)

  standata <- list(
    X = x_vec,
    Y = y_vec,
    K = length(x),
    Nx = length(x_vec),
    Ny = length(y_vec),
    Jx = vapply(x, length, numeric(1)),
    Jy = vapply(y, length, numeric(1))
  )

  if (!is.null(z)) {
    standata$z <- z
    standata$Nz <- length(z)
  }

  standata

}

add_priors <- function(standata, priors) {
  # Check that necessary priors have been specified:
  stopifnot(
    "sigma_x_max, sigma_y_max, alpha_var, and beta_var must all be specified" =
      all(c("sigma_x_max", "sigma_y_max", "alpha_var", "beta_var") %in% names(priors)),
    "sigma_x_max must be positive" = (0 < priors$sigma_x_max & priors$sigma_x_max < Inf),
    "sigma_y_max must be positive" = (0 < priors$sigma_y_max & priors$sigma_y_max < Inf),
    "alpha_var must be positive" = (0 < priors$alpha_var & priors$alpha_var < Inf),
    "alpha_var must be positive" = (0 < priors$beta_var & priors$beta_var < Inf)
  )

  if ("alpha_mean" %in% names(priors)) {
    stopifnot("alpha_mean must be finite" = abs(priors$alpha_mean) < Inf)
  } else {
    priors$alpha_mean <- 0
  }

  if ("beta_mean" %in% names(priors)) {
    stopifnot("beta_mean must be finite" = abs(priors$beta_mean) < Inf)
  } else {
    priors$beta_mean <- 0
  }

  if ("power" %in% names(priors)) {
    stopifnot("power prior specified, but no additional data (z)" = "z" %in% names(standata))
    stopifnot("power prior must be between 0 and 1 (inclusive)" = (0 <= priors$power & priors$power <= 1))
  }

  if (any(c("power_a", "power_b") %in% names(priors))) {
    stopifnot(
      "Parameters power_a and power_b must either be specified together or not at all" =
        all(c("power_a", "power_b") %in% names(priors))
      )

    stopifnot("power_a and prior_b specified, but no additional data (z)" = "z" %in% names(standata))
    stopifnot("power and power_a/b cannot both be specified" = ! "power" %in% names(priors))
  }

  # If no power prior specified, use uniform
  if ("z" %in% names(standata) & !any(c("power", "power_a") %in% names(priors))) {
    priors$power_a <- 1
    priors$power_b <- 1
  }

  c(standata, priors)
}
