#' Calculate values of Constant Protected Income Elasticity Evaluations (CPIEE)
#'
#' This function calculates the value of \eqn{g(x) = \frac{(\ln\frac{x}{c})^{1-\nu}}{1-\nu}} given a value for \eqn{\nu \geq 0} and \eqn{\nu \neq 1} or will calculate the value of \ln\frac{x}{c} given a value \eqn{\nu = 1}.
#'
#'
#' @param x A numeric value or list.
#' @param nu The value of nu.
#' @param c The value of c.
#' @return A numeric or list of return values from the CPIEE utility function.
#' @export

cpiee = function(x, nu, c) {
  if (c > x) {
    val = "C must be greater than X"
  }
  if (nu < 0) {
    val = "Please use nu >= 0."
  } else if (nu != 1) {
    val = (log(x / c))^(1 - nu) / (1 - nu)
  } else if (nu == 1) {
    val = log(x / c)
  }
  return(val)
}

