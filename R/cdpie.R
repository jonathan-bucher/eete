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
    val = "Please provide a value for a or L."

  } else if (!is.null(a)){
    if (a > 0){
      val = -exp(-a*x)
    }else{
      val = "Please use a > 0"
    }

  } else if (!is.null(L)){
    if (L > 0){
      val = -2^(-x/L)
    }else{
      val = "Please use L > 0"
    }
  }
  return(val)
}


#' Calculate inverse values of Constant Difference Protected Income Evaluations (CDPIE), or Kolm-Pollak.
#'
#' This function calculates the values of the inverse of the CDPIE function.
#'
#'
#' @param y A numeric value or list.
#' @param a The value of a, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if a is provided.
#' @return A numeric or list of return values from the inverse of the CDPIE utility function.
#' @export

cdpie_inv = function(x, a = NULL, L = NULL){
  if (is.null(a) & is.null(L)){
    val = "Please provide a value for a or L."

  } else if (!is.null(a)){
    if (a > 0){
      val = -log(-x)/a
    }else{
      val = "Please use a > 0"
    }

  } else if (!is.null(L)){
    if (L > 0){
      val = -L*log2(-x)
    }else{
      val = "Please use L > 0"
    }
  }
  return(val)
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
    val = "Please provide a value for a or L."

  } else if (!is.null(a)){
    if (a > 0){
      val = -1/(a*x)
    }else{
      val = "Please use a > 0"
    }

  } else if (!is.null(L)){
    if (L > 0){
      val = -L/(log(2)*x)
    }else{
      val = "Please use L > 0"
    }
  }
  return(val)
}
