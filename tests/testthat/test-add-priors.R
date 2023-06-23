test_that("Prior logic works", {

  get_priors <- function(priors, power, theta_range) {
    BayesDeming:::add_priors(standata = standata, priors = priors, power = power, theta_range = theta_range)
  }

  df <- sim_data(
    Jx = c(6, 6, 6, 6, 6, 6, 6, 6),
    Jy = c(4, 5, 5, 4, 5, 5, 5, 7),
    theta = 1:8,
    sdx = 1, sdy = 1,
    alpha = 0, beta = 1
  )

  standata <- BayesDeming:::parse_df(df, y ~ x, x0 = NULL)

  priors <- list(
    sigma_x_min = 10,
    sigma_y_min = 10,
    sigma_x_max = 10,
    sigma_y_max = 10,
    alpha_mean = 0,
    alpha_sd = 10,
    beta_mean = 0,
    beta_sd = 10
  )

  # Basic parsing of priors works
  res <- BayesDeming:::add_priors(standata = standata, priors = priors, power = NULL, theta_range = NULL)
  expect_equal(res, c(standata, priors))

  # Certain prior specifications are required
  priors_wrong <- priors
  priors_wrong$sigma_x_max <- NULL
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_wrong, power = NULL, theta_range = NULL),
    regexp = "must all be specified"
  )

  # Requirements for certain parameters
  priors_wrong <- priors
  priors_wrong$sigma_x_max <- -5
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_wrong, power = NULL, theta_range = NULL),
    regexp = "sigma\\_x\\_max must be positive"
  )

  priors_wrong <- priors
  priors_wrong$sigma_y_max <- -5
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_wrong, power = NULL, theta_range = NULL),
    regexp = "sigma\\_y\\_max must be positive"
  )

  priors_wrong <- priors
  priors_wrong$alpha_sd <- -1
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_wrong, power = NULL, theta_range = NULL),
    regexp = "alpha\\_sd must be positive"
  )

  priors_wrong <- priors
  priors_wrong$beta_sd <- -1
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_wrong, power = NULL, theta_range = NULL),
    regexp = "beta\\_sd must be positive"
  )

  # sigma_x/y_min and alpha/beta_mean default to 0 if not provided
  priors_minimum <- list(
    sigma_x_max = 10,
    sigma_y_max = 10,
    alpha_sd = 10,
    beta_sd = 10
  )

  # certain specifications can be missing
  res <- BayesDeming:::add_priors(standata = standata, priors = priors_minimum, power = NULL, theta_range = NULL)
  expect_equal(res$sigma_x_min, 0)
  expect_equal(res$sigma_y_min, 0)
  expect_equal(res$alpha_mean, 0)
  expect_equal(res$beta_mean, 0)


  # power priors require additional specifications
  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors, power = "point", theta_range = NULL),
    regexp = "priors\\$\\w* must be provided if"
  )

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors, power = "beta", theta_range = NULL),
    regexp = "priors\\$\\w* must be provided if"
  )

  #
  # Nothing to test for power = "unif"; no specs required
  #

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors, power = "normal", theta_range = NULL),
    regexp = "priors\\$\\w* must be provided if"
  )



  # power priors require additional data
  priors_point <- priors; priors_point$power <- .5
  priors_beta <- priors; priors_beta$power_a <- 1; priors_beta$power_b <- 2
  priors_unif <- priors
  priors_normal <- priors; priors_normal$power_mean <- .5; priors_normal$power_sd <- .2

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_point, power = "unif", theta_range = NULL),
    regexp = "but no additional data"
  )

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_beta, power = "beta", theta_range = NULL),
    regexp = "but no additional data"
  )

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_unif, power = "unif", theta_range = NULL),
    regexp = "but no additional data"
  )

  expect_error(
    BayesDeming:::add_priors(standata = standata, priors = priors_normal, power = "normal", theta_range = NULL),
    regexp = "but no additional data"
  )

  # power priors work when everything is specified
  standata$z <- 1:5

  res_point <- BayesDeming:::add_priors(standata = standata, priors = priors_point, power = "point", theta_range = NULL)
  expect_equal(res_point$power, .5)

  res_beta <- BayesDeming:::add_priors(standata = standata, priors = priors_beta, power = "beta", theta_range = NULL)
  expect_equal(res_beta$power_a, 1)
  expect_equal(res_beta$power_b, 2)

  res_unif <- BayesDeming:::add_priors(standata = standata, priors = priors_unif, power = "unif", theta_range = NULL)
  expect_equal(res_unif$power_a, 1)
  expect_equal(res_unif$power_b, 1)

  res_normal <- BayesDeming:::add_priors(standata = standata, priors = priors_normal, power = "normal", theta_range = NULL)
  expect_equal(res_normal$power_mean, .5)
  expect_equal(res_normal$power_sd, .2)

})







