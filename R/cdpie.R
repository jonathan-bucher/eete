#' Calculate values of Constant Difference Protected Income Evaluations (CDPIE), or Kolm-Pollak.
#'
#' This function calculates the value of \eqn{g(x) = -e^{-\frac{x}{r}}} given a value for \eqn{r} or will calculate the value of \eqn{g(x) = -2^{-\frac{x}{L}}} given a value for \eqn{L}.
#'
#'
#' @param x A numeric value or list.
#' @param r The value of r, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if r is provided.
#' @return A numeric or list of return values from the CDPIE utility function.
#' @export

cdpie = function(x, r = NULL, L = NULL){
  if (is.null(r) & is.null(L)){
    val = "Please provide a value for r or L."

  } else if (!is.null(r)){
    if (r != 0){
      val = -exp(-x/r)
    }else{
      val = "Please use r > 0"
    }

  } else if (!is.null(L)){
    if (L != 0){
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
#' @param r The value of r, a scale factor that adjusts the intensity of risk aversion. This parameter is optional if L is provided.
#' @param L The value of L. This parameter is optional if r is provided.
#' @return A numeric or list of return values from the inverse of the CDPIE utility function.
#' @export

cdpie_inv = function(x, r = NULL, L = NULL){
  if (is.null(r) & is.null(L)){
    val = "Please provide a value for r or L."

  } else if (!is.null(r)){
    if (r != 0){
      val = -r*log(-x)
    }else{
      val = "Please use r > 0"
    }

  } else if (!is.null(L)){
    if (L != 0){
      val = -L*log2(-x)
    }else{
      val = "Please use L > 0"
    }
  }
  return(val)
}
