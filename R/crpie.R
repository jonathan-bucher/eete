#' Calculate values of Constant Relative Protected Income Evaluations (CRPIE), or Kolm-Atkinson.
#'
#' This function calculates the value of \eqn{g(x) = \frac{x^{1-\gamma}}{1-\gamma}} given a value for \eqn{\gamma} or will calculate \eqn{g(x) = \log_2(k) x^{\frac{1}{\log_2(k)}}} if given a value for \eqn{k}.
#' If \eqn{\gamma = 1} or \eqn{k = \infty}, the function will return \eqn{\log(x)}.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion.
#' @param k The value of k, the percent of income to protect.
#' @return A numeric or list of return values from the CRPIE utility function.
#' @export

crpie = function(x, gam = NULL, k = NULL){
  # check for negative values in x vector
  neg = FALSE
  if (any(x < 0)){
    neg = TRUE
  }
  if (is.null(gam) & is.null(k)){
    stop("Please provide a value for gamma or k.")
  }
  if (!is.null(gam) & !is.null(k)){
    stop("Please do not specify both gamma and k.")
  }
  if (!is.null(gam)){
    if (gam < 0){
      stop("Values for gamma must be positive.")
    }
    if (gam != 1){
      return(x^(1-gam)/(1-gam))
    }
    if (neg) {
      stop("for cases where gamma is specified to be a value other than one, all values of x must be positive")
    }
    return(log(x))
  }
  else {
    if (k < 0){
      stop("k must be positive")
    }
    if (k == 0){
      if (neg){
        stop("for cases where k is specified as 0, all values of x must be positive")
      }
      return(log(x))
    }
    if (k >= 1 & k < 2){
      stop("Values for k between 1 (inclusive) and 2 (exclusive) are not allowed.")
    }
    else{
      return(log2(k) * x ^ (1/(log2(k))))
    }
  }
}

#' Calculate inverse values of Constant Relative Protected Income Evaluations (CRPIE), or Kolm-Atkinson.
#'
#' This function calculates the inverse values of CRPIE.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion.
#' @param k The value of k, the percent of income to protect.
#' @return A numeric or list of return values from the inverse CRPIE utility function.
#' @export

crpie_inv = function(x, gam = NULL, k = NULL){
  if (is.null(gam) & is.null(k)){
    stop("Please provide a value for gamma or k.")
  }
  if (!is.null(gam) & !is.null(k)){
    stop("Please do not specify both gamma and k")
  }
  if (!is.null(gam)){
    if (gam < 0){
      stop("Values for gamma must be positive.")
      }
    if (gam != 1){
      return((x*(1-gam))^(1/(1-gam)))
    }
    return(exp(x))
  }
  else {
    if (k == 0){
      val = exp(x)
      }
    else if (k >= 1 & k < 2){
      stop("Values for k between 1 and 2 are not allowed.")
      }
    else{
      val = (x/(log2(k)))^(log2(k))
    }
  }
  return(val)
}




#' Calculate inverse prime values of Constant Relative Protected Income Evaluations (CRPIE), or Kolm-Atkinson.
#'
#' This function calculates the inverse prime values of CRPIE.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion. This value is optional if a value for k is provided.
#' @param k The value of k, the percent of income to protect. This value is optional if a value for gamma is provided.
#' @return A numeric or list of return values from the inverse prime CRPIE utility function.
#' @export

crpie_inv_prime = function(x, gam = NULL, k = NULL){
  if (is.null(gam) & is.null(k)){
    stop("Please provide a value for gamma or k.")
  }
  if (!is.null(gam) & !is.null(k)){
    stop("Please do not provide a value for both gamma and k")
  }
  if (!is.null(gam)){
    if (gam < 0){
      stop("Values for gamma must be positive.")
    }
    if (gam != 1){
      return((x*(1-gam))^(gam/(1-gam)))
    }
    return(exp(x))
  }
  else {
    if (k < 0){
      stop("k must be positive")
    }
    if (k == 0){
      return(exp(x))
    }
    if (k >= 1 & k < 2){
      stop("Values for k between 1 (inclusive) and 2 (exclusive) are not allowed.")
    }
    return((x/(log2(k)))^(log2(k)-1))
  }
}
