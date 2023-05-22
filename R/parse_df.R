# Helper function, convert nested df into data list for stan
parse_df <- function(df, formula, x0) {

  # Convert to list column format if in long-form
  if (!any(vapply(df, is.list, logical(1)))) {

    stopifnot("value" %in% colnames(df), "`df` in long-form format and is missing `value` column")
    stopifnot("grp" %in%   colnames(df), "`df` in long-form format and is missing `grp` column")
    stopifnot("var" %in%   colnames(df), "`df` in long-form format and is `var` column")

    df <- tidyr::pivot_wider(df, values_from = value, names_from = var, values_fn = list)
    df <- dplyr::select(df, !grp)

  }

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

  # stan models are written with z, not x0
  if (!is.null(x0)) {
    standata$z <- x0
    standata$Nz <- length(x0)
  }

  standata

}

add_priors <- function(standata, priors, power, theta_range) {

  # Set defaults if missing
  if (! "sigma_x_min" %in% names(priors)) priors$sigma_x_min <- 0
  if (! "sigma_y_min" %in% names(priors)) priors$sigma_y_min <- 0

  if (! "alpha_mean" %in% names(priors)) priors$alpha_mean <- 0
  if (! "beta_mean" %in% names(priors)) priors$beta_mean <- 0

  # Check that necessary priors have been specified:
  stopifnot(
    "sigma_x_max, sigma_y_max, alpha_sd, and beta_sd must all be specified" =
      all(c("sigma_x_max", "sigma_y_max", "alpha_sd", "beta_sd") %in% names(priors)),
    "sigma_x_max must be positive" = (0 < priors$sigma_x_max & priors$sigma_x_max < Inf),
    "sigma_y_max must be positive" = (0 < priors$sigma_y_max & priors$sigma_y_max < Inf),
    "alpha_sd must be positive" = (0 < priors$alpha_sd & priors$alpha_sd < Inf),
    "beta_sd must be positive" = (0 < priors$beta_sd & priors$beta_sd < Inf)
  )

  if (!is.null(theta_range)) {
    stopifnot(
      "theta_range must be a numeric vector with length 2" = (length(theta_range) == 2),
      "theta_range[1] must be less than theta_range[2]" = (theta_range[1] < theta_range[2])
    )

    if (any(standata$X < theta_range[1] | standata$X > theta_range[2])) warning("Observed x values outside of theta_range, consider widening")

    priors$theta_min <- theta_range[1]
    priors$theta_max <- theta_range[2]
  }

  if (!is.null(power)) {

    if (power == "point") {
      stopifnot("priors$power must be provided if power = 'point'" = "power" %in% names(priors))
      stopifnot("power prior specified, but no additional data (x0)" = "z" %in% names(standata))
      stopifnot("power prior must be between 0 and 1 (inclusive)" = (0 <= priors$power & priors$power <= 1))
    }

    if (power == "beta") {
      stopifnot("priors$power_a and priors$power_b must be provided if power = 'beta'" = all(c("power_a", "power_b") %in% names(priors)))
      stopifnot("power_a and prior_b specified, but no additional data (x0)" = "z" %in% names(standata))
    }

    if (power == "normal") {
      stopifnot("priors$power_mean and priors$power_sd must be provided if power = 'normal'" = all(c("power_mean", "power_sd") %in% names(priors)))
      stopifnot("power_mean and prior_sd specified, but no additional data (x0)" = "z" %in% names(standata))
    }

    if (power == "unif") {
      stopifnot("power = 'unif' specified, but no additional data (x0)" = "z" %in% names(standata))
      priors$power_a <- 1
      priors$power_b <- 1
    }

  }

  c(standata, priors)
}
