#' Simulate Deming data
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
