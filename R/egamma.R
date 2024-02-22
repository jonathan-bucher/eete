#' Calculate values of the gamma function.
#'
#' This function calculates the value of \eqn{g(x) = \frac{x^{1-\gamma}}{1-\gamma}} given a value for \eqn{\gamma}.
#' If \eqn{\gamma = 1}, the function will return \eqn{\log(x)}.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma.
#' @return A numeric or list of return values from the gamma function.
#' @export

egamma = function(x, gam){
  if (gam != 1){
    val = x^(1-gam)/(1-gam)
  }else{
    val =log(x)
  }
  return(val)
}
