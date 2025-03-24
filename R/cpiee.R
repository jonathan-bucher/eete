#' Calculate values of Constant Protected Income Elasticity Evaluations (CPIEE)
#'
#' This function calculates the value of \eqn{g(x) = \frac{(\ln\frac{x}{c})^{1-\eta}}{1-\eta}} given a value for \eqn{\eta \geq 0} and \eqn{\eta \neq 1} or will calculate the value of \eqn{\ln\frac{x}{c}} given a value \eqn{\eta = 1}.
#'
#'
#' @param x A numeric value or list.
#' @param eta The value of eta.
#' @param c The value of c.
#' @return A numeric or list of return values from the CPIEE utility function.
#' @export

cpiee = function(x, eta, c) {
  if (!all(x > c,  na.rm = TRUE)) {
    stop("All values of x must be greater than c")
  }
  if (eta < 0) {
    stop("Please use eta >= 0")
  }
  if (c == 0){
    stop("please use nonzero c value")
  }
  if (eta != 1) {
    return((log(abs(x) / c))^(1 - eta) / (1 - eta))
  }
  else {
    return(log(log(abs(x) / c)))
  }
}


#' Calculate inverse values of Constant Protected Income Elasticity Evaluations (CPIEE)
#'
#'
#' @param x A numeric value or list.
#' @param eta The value of eta.
#' @param c The value of c.
#' @return A numeric or list of return values from the inverse CPIEE utility function.
#' @export

cpiee_inv = function(x, eta, c) {

  if (eta < 0) {
    stop("Please use eta >= 0.")
  }
  if (eta != 1) {
    return(c * exp((x * (1 - eta))^(1 / (1 - eta))))
  }
  else {
    return(c * exp(exp(x)))
  }
}



#' Calculate inverse prime values of Constant Protected Income Elasticity Evaluations (CPIEE)
#'
#'
#' @param x A numeric value or list
#' @param eta The value of eta
#' @param c The value of c
#' @return A numeric or list of return values from the inverse prime CPIEE utility function.
#' @export

# need to verify parameter requirements
cpiee_inv_prime = function(x, eta, c) {

  if (eta < 0) {
    stop("Please use eta >= 0")
  }
  if (eta != 1) {
    return(c * (x * (1 - eta)) ^ ((1 / (1 - eta)) - 1) * exp((x * (1 - eta)) ^ (1 / (1 - eta))))
  }
  if (eta == 1){
    return(c * exp(x + exp(x)))
  }
}
