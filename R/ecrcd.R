#' Calculate values of the Constant Relative Collateral Damage (CRCD), or Kolm-Pollak.
#'
#' This function calculates the value of \eqn{g(x) = \frac{x^{1-\gamma}}{1-\gamma}} given a value for \eqn{\gamma} or will calculate \eqn{g(x) = \log_2(k) x^{\frac{1}{\log_2(k)}}} if given a value for \eqn{k}.
#' If \eqn{\gamma = 1} or \eqn{k = \infty}, the function will return \eqn{\log(x)}.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion. This value is optional if a value for k is provided.
#' @param k The value of k, the percent of income to protect. This value is optional if a value for gamma is provided.
#' @return A numeric or list of return values from the CRRA utility function.
#' @export

ecrcd = function(x, gam = NULL, k = NULL){
  if (is.null(gam) & is.null(k)){
    val = "Please provide a value for gamma or k."

  } else if (!is.null(gam)){
    if (gam != 1){
      val = x^(1-gam)/(1-gam)
    }else{
      val =log(x)
    }

  } else if (!is.null(k)){
    if (k != Inf){
      val = log2(k) * x^(1/(log2(k)))
    }else{
      val =log(x)
    }
  }
  return(val)
}
