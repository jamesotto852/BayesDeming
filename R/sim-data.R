#' Simulate Deming data
#'
#' @param Jx,Jy numeric vectors of equal lengths, the number of repeated measures per group.
#' @param theta numeric vector the same length as `Jx` and `Jy`, the X values group means
#' @param sdx,sdy the standard deviation of the X and Y values across all groups
#' @param lambda the ratio of `sdy`/`sdx`. Either `sdy` or `lambda` must be specified, but not both.
#' @param alpha,beta the values of the intercept and slope relating X and Y group means.
#'
#' @export
#' @importFrom tibble tibble
#' @examples
#' df <- sim_data(
#'   Jx = c(5, 5, 5, 5, 5, 7, 4, 4),
#'   Jy = c(6, 6, 6, 6, 6, 6, 6, 6),
#'   sdx = sqrt(((1/50)/2)^.5), lambda = 2
#' )
sim_data <- function(Jx, Jy, theta = NULL, sdx, sdy = NULL, lambda = NULL, alpha = 0, beta = 1) {

  stopifnot("Jx and Jy must be equal lengths" = (length(Jx) == length(Jy)))
  K <- length(Jx)

  if (is.null(theta)) theta <- 1:K

  stopifnot("Exactly one of lambda or sdy must be specified" = xor(is.null(lambda), is.null(sdy)))
  if (is.null(sdy)) sdy <- sqrt(lambda)*sdx


  tibble::tibble(
    x = lapply(1:K, function(k) rnorm(Jx[k], theta[k], sdx)),
    y = lapply(1:K, function(k) rnorm(Jy[k], alpha + beta * theta[k], sdy))
  )

}
