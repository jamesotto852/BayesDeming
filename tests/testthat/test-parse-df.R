test_that("Parsing works", {

  Jx <- c(6, 6, 6, 6, 6, 6, 6, 6)
  Jy <- c(4, 5, 5, 4, 5, 5, 5, 7)

  df <- sim_data(
    Jx = Jx,
    Jy = Jy,
    theta = 1:8,
    sdx = 1, sdy = 1,
    alpha = 0, beta = 1
  )

  res <- BayesDeming:::parse_df(df, y ~ x, x0 = NULL)

  res_expected <- list(
    X = unlist(df$x),
    Y = unlist(df$y),
    K = length(Jx),
    Nx = sum(Jx),
    Ny = sum(Jy),
    Jx = Jx,
    Jy = Jy
  )

  expect_equal(res, res_expected)

})

test_that("Pivoting works", {

  df <- data.frame(
    value = rep(1:8, each = 10),
    var = rep(c("a", "b"), times = 40),
    grp = rep(1:8, each = 10)
  )

  df <- df[-1, ]

  res <- BayesDeming:::parse_df(df, b ~ a, x0 = NULL)

  res_expected <- list(
    X = rep(1:8, each = 5)[-1],
    Y = rep(1:8, each = 5),
    K = 8,
    Nx = 39,
    Ny = 40,
    Jx = c(4, rep(5, length = 7)),
    Jy = rep(5, length = 8)
  )

  expect_equal(res, res_expected)
})
