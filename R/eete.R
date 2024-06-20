#' Find the Inverse Function Value
#'
#' This function returns the inverse function value of a given function `f` for a specific value `y`,
#' searching within the interval specified by `lower` and `upper`.
#'
#' @param f A function for which the inverse value needs to be found.
#' @param lower The lower bound of the search interval.
#' @param upper The upper bound of the search interval.
#' @return A function that takes a single value `y` and returns the inverse function value of `f` at `y`.
#' @examples
#' inv_sqrt <- inverse_fun(function(x) x^2, 0, 100)
#' inv_sqrt(4) # Should find a value close to 2


inverse_fun = function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}

#' Estimate the Egalitarian Equivalent Treatment Effect (eete)
#'
#' This function estimates the eete, which optional arguments to include an IV or bootstrapped SE.
#' It applies an input function to data, conditioning on treatment (and instrument variables, if applicable).
#'
#' @param inputFunction A function that will be applied to the data.
#' @param ... Additional arguments to be passed to `inputFunction`.
#' @param y The name of the outcome variable column in `data`.
#' @param d The name of the treatment variable column in `data`.
#' @param z Optional argument to include the name of the instrument variable column in `data`.
#' @param data The data frame containing the variables.
#' @param lower The lower bound for finding the inverse function value.
#' @param upper The upper bound for finding the inverse function value.
#' @param se Optional argument to calculate the standard error of the estimate with bootstrapping.
#' @param B The number of iterations to be used in the standard error bootstrap.
#' @return The estimated egalitarian equivalent treatment effect.
#' @import dplyr
#' @importFrom boot boot
#' @export
#' @examples
#' # Example using the mtcars dataset
#' # Let's assume `mpg` is the outcome, `vs` is the treatment indicator, and `am` is used as an instrument.
#' eete_sqrt = eete(sqrt, y = "mpg", d = "vs", z = "am", data = mtcars)
#' print(eete_sqrt)

eete = function(inputFunction, ..., y, d, z = NULL, data, lower = 0.1, upper = 100, se = FALSE, B = 1000, inv = NULL) {

  data_test = data %>%
    dplyr::select(!!sym(y))

  if (is.character(inputFunction(data_test, ...))) {
    eete = inputFunction(data_test, ...)

  } else {

    if (is.null(inv)) {
      fun_inverse = inverse_fun(function(x) inputFunction(x, ...), lower, upper)
    } else {
      fun_inverse = function(x) {
        return(list(root = inv(x, ...)))
      }
    }

  if (!is.null(z)) {

  ee = function(data, indices){

    bdata = data[indices,]

    data1 = bdata %>%
      dplyr::filter(!!sym(z) == 1)

    data0 = bdata %>%
      dplyr::filter(!!sym(z) == 0)

    yz1 = data1[[y]]
    yz0 = data0[[y]]
    p1 = mean(data1[[d]], na.rm = TRUE)
    p0 = mean(data0[[d]], na.rm = TRUE)

    fee_p1 = mean(inputFunction(yz1, ...), na.rm = TRUE)
    fee_p0 = mean(inputFunction(yz0, ...), na.rm = TRUE)
    fee1 = (((1 - p0) * fee_p1 - (1 - p1) * fee_p0) / (p1 - p0))
    fee0 = ((p1 * fee_p0 - p0 * fee_p1) / (p1 - p0))

    eete = (fun_inverse(fee1)$root - fun_inverse(fee0)$root)

    return(eete)
  }



  } else {

    ee = function(data, indices){

      bdata = data[indices,]

      data1 = bdata %>%
        dplyr::filter(!!sym(d) == 1)

      data0 = bdata %>%
        dplyr::filter(!!sym(d) == 0)

      yz1 = data1[[y]]
      yz0 = data0[[y]]

      fee1 = mean(inputFunction(yz1, ...), na.rm = TRUE)
      fee0 = mean(inputFunction(yz0, ...), na.rm = TRUE)

      eete = (fun_inverse(fee1)$root - fun_inverse(fee0)$root)

      return(eete)
    }

  }

  eete = ee(data, 1:nrow(data))

  if (se == TRUE) {

    boot_results = boot::boot(data, ee, R = B)
    se = sd(boot_results$t)

    eete = list(estimate = eete, se = se)

  } else {

    eete = list(estimate = eete)
  }
}
  return(eete)
}
