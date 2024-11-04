#' Calculate values of Constant Relative Protected Income Evaluations (CRPIE), or Kolm-Atkinson.
#'
#' This function calculates the value of \eqn{g(x) = \frac{x^{1-\gamma}}{1-\gamma}} given a value for \eqn{\gamma} or will calculate \eqn{g(x) = \log_2(k) x^{\frac{1}{\log_2(k)}}} if given a value for \eqn{k}.
#' If \eqn{\gamma = 1} or \eqn{k = \infty}, the function will return \eqn{\log(x)}.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion. This value is optional if a value for k is provided.
#' @param k The value of k, the percent of income to protect. This value is optional if a value for gamma is provided.
#' @return A numeric or list of return values from the CRPIE utility function.
#' @export

crpie = function(x, gam = NULL, k = NULL){
  if (is.null(gam) & is.null(k)){
    val = "Please provide a value for gamma or k."

  } else if (!is.null(gam)){
    if (gam < 0){
      val = "Values for gamma must be positive."
    } else if (gam != 1){
      val = x^(1-gam)/(1-gam)
    }else{
      val =log(x)
    }

  } else {
    if (k == 0){
      val = log(x)
    }else if (k >= 1 & k < 2){
      val = "Values for k between 1 and 2 are not allowed."
    }else{
      val = log2(k) * x^(1/(log2(k)))
    }
  }
  return(val)
}

#' Calculate inverse values of Constant Relative Protected Income Evaluations (CRPIE), or Kolm-Atkinson.
#'
#' This function calculates the inverse values of CRPIE.
#'
#' Note: If \eqn{k < 1}, \eqn{k} represents the percent of income to protect. If \eqn{k > 1}, \eqn{k} represents the multiple person A must receive to sacrifice income of person B completely.
#'
#' @param x A numeric value or list.
#' @param gam The value of gamma, the coefficient of relative risk aversion. This value is optional if a value for k is provided.
#' @param k The value of k, the percent of income to protect. This value is optional if a value for gamma is provided.
#' @return A numeric or list of return values from the inverse CRPIE utility function.
#' @export

crpie_inv = function(x, gam = NULL, k = NULL){
  if (is.null(gam) & is.null(k)){
    val = "Please provide a value for gamma or k."

  } else if (!is.null(gam)){
    if (gam < 0){
      val = "Values for gamma must be positive."

      } else if (gam != 1){
      val = (x*(1-gam))^(1/(1-gam))
    }else{
      val =exp(x)
    }

  } else {
    if (k == 0){
      val = exp(x)

      }else if (k >= 1 & k < 2){
      val = "Values for k between 1 and 2 are not allowed."

      }else{
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
    val = "Please provide a value for gamma or k."

  } else if (!is.null(gam)){
    if (gam < 0){
      val = "Values for gamma must be positive."

    } else if (gam != 1){
      val = (x*(1-gam))^(gam/(1-gam))

    }else{
      val = exp(x)
    }

  } else {
    if (k == 0){
      val = exp(x)

    }else if (k >= 1 & k < 2){
      val = "Values for k between 1 and 2 are not allowed."

    }else{
      val = (x/(log2(k)))^(log2(k)-1)
    }
  }
  return(val)
}
