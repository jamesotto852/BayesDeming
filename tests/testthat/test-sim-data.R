test_that("data simulation logic works", {

  # Everything specified correctly
  Jx <- c(6, 6, 6, 6, 6, 6, 6, 6)
  Jy <- c(4, 5, 5, 4, 5, 5, 5, 7)

  set.seed(1)
  df <- sim_data(
    Jx = Jx,
    Jy = Jy,
    theta = 1:8,
    sdx = 1, sdy = 1,
    alpha = 0, beta = 1
  )

  expect_s3_class(df, "tbl_df")
  expect_equal(ncol(df), 2)
  expect_equal(colnames(df), c("x", "y"))
  expect_equal(nrow(df), length(Jx))


  Nx <- length(Reduce(c, df$x))
  Ny <- length(Reduce(c, df$y))
  expect_equal(Nx, sum(Jx))
  expect_equal(Ny, sum(Jy))

  # theta not required (defaults to 1:(no. of groups))
  set.seed(1)
  df_no_theta <- sim_data(
    Jx = Jx,
    Jy = Jy,
    sdx = 1, sdy = 1,
    alpha = 0, beta = 1
  )

  expect_equal(df_no_theta, df)

  # lambda xor sdy must be specified
  expect_error({
    sim_data(
      Jx = Jx,
      Jy = Jy,
      sdx = 1,
      alpha = 0, beta = 1
    )
  }, "Exactly one of lambda or sdy"
  )

  expect_error({
    sim_data(
      Jx = Jx,
      Jy = Jy,
      sdx = 1, sdy = 1, lambda = 1,
      alpha = 0, beta = 1
    )
  }, "Exactly one of lambda or sdy"
  )

  set.seed(1)
  df_no_sdy <- sim_data(
    Jx = Jx,
    Jy = Jy,
    sdx = 1, lambda = 1,
    alpha = 0, beta = 1
  )

  expect_equal(df_no_theta, df)

})
