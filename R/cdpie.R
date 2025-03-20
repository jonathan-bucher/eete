#' Calculate values of Constant Difference Protected Income Evaluations (CDPIE), or Kolm-Pollak.
#'
#' This function calculates the value of \eqn{g(x) = -e^{-\frac{x}{a}}} given a value for \eqn{a} or will calculate the value of \eqn{g(x) = -2^{-\frac{x}{L}}} given a value for \eqn{L}.
#'
#'
#' @param x A numeric value or list.
#' @param a The value of a, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if a is provided.
#' @return A numeric or list of return values from the CDPIE utility function.
#' @export

cdpie = function(x, a = NULL, L = NULL){
  if (is.null(a) & is.null(L)){
    stop("Please provide a value for a or L.")
  }
  if (!is.null(a) & !is.null(L)){
    stop("Please specify a value for only a or L, not both")
  }
  if (!is.null(a)){
    if (a > 0){
      return(-exp(-a*x))
    }
    else{
      stop("Please use a > 0")
    }
  }
  else{
    if (L > 0){
      return(-2^(-x/L))
    }
    else{
      stop("Please use L > 0")
    }
  }
}


#' Calculate inverse values of Constant Difference Protected Income Evaluations (CDPIE), or Kolm-Pollak.
#'
#' This function calculates the values of the inverse of the CDPIE function.
#'
#'
#' @param x A numeric value or list.
#' @param a The value of a, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if a is provided.
#' @return A numeric or list of return values from the inverse of the CDPIE utility function.
#' @export

cdpie_inv = function(x, a = NULL, L = NULL){
  if (any(x > 0)){
    stop("All values of x must be less than 0")
  }
  if (is.null(a) & is.null(L)){
    stop("Please provide a value for a or L.")
  }
  if (!is.null(a) & !is.null(L)){
    stop("Please specify a value for only a or L, not both")
  }
  if (!is.null(a)){
    if (a > 0){
      return(-log(-x)/a)
    }
    else{
      stop("Please use a > 0")
    }
  }
  else{
    if (L > 0){
      return(-L*log2(-x))
    }
    else{
      stop("Please use L > 0")
    }
  }
}

#' Calculate inverse prime values of Constant Difference Protected Income Evaluations (CDPIE), or Kolm-Pollak.
#'
#' This function calculates the values of the inverse prime of the CDPIE function.
#'
#'
#' @param y A numeric value or list.
#' @param a The value of a, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if a is provided.
#' @return A numeric or list of return values from the inverse prime of the CDPIE utility function.
#' @export

cdpie_inv_prime = function(x, a = NULL, L = NULL){
  if (is.null(a) & is.null(L)){
    stop("Please provide a value for a or L.")
  }
  if(!is.null(a) & !is.null(L)){
    stop("Please specify a value for only a or L, not both")
  }
  else if (!is.null(a)){
    if (a > 0){
      return(-1/(a*x))
    }
    else{
      stop("Please use a > 0")
    }
  }
  else if (!is.null(L)){
    if (L > 0){
      return(-L/(log(2)*x))
    }
    else{
      stop("Please use L > 0")
    }
  }
}
