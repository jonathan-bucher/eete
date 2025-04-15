# This script contains functions for computing the egalitarian equivalent treatment effect

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
#' @export

inverse_fun = function(f, lower, upper) {
  function(y) uniroot((function(x) f(x) - y), lower = lower, upper = upper)$root
}




#' Estimate the Egalitarian Equivalent Treatment Effect (eete) with Instrumental Variables
#'
#' This function estimates the eete, which optional arguments to include an IV or bootstrapped SE.
#' It applies an input function to data, conditioning on treatment (and instrument variables, if applicable).
#'
#' @param inputFunction A function that will be applied to the data.
#' @param ... Additional arguments to be passed to `inputFunction`.
#' @param y The name of the outcome variable column in `data`.
#' @param d The name of the treatment variable column in `data`.
#' @param z Argument to include the name of the instrument variable column in `data`. z must only take values 0 or 1
#' @param data The data frame containing the variables.
#' @param f_inv Inverse function of f
#' @return A list containing the estimated egalitarian equivalent treatment effect and the estimated variance.
#' @import dplyr
#' @importFrom boot boot
#'
#' @export

iv_eete = function(f, ..., y, d, z, data, indices, f_inv){
  # create the option of using a bootstrap sample
  bdata = data[indices,]
  # check if z is binary
  if (!all(bdata[[z]] %in% c(0, 1))) {
    stop("instrumental variable z must only take values 0 or 1")
  }
  if (!all(bdata[[d]] %in% c(0, 1))) {
    stop("treatment indicator must only take values 0 (treatment) or 1 (control)")
  }
  # ensure there are individuals in treatment and control groups
  if ((all(bdata[[d]] == 1)) | (all(bdata[[d]] == 0))){
    stop("the treatment and control groups must each have at least one observation")
  }
  # filter data based on value of instrument
  data1 = bdata %>%
    dplyr::filter(!!sym(z) == 1)
  data0 = bdata %>%
    dplyr::filter(!!sym(z) == 0)
  # extracting outcome variables for both groups
  yz1 = data1[[y]]
  yz0 = data0[[y]]
  # compute proportion of treated individuals (d) in both groups
  p1 = mean(data1[[d]], na.rm = TRUE)
  p0 = mean(data0[[d]], na.rm = TRUE)
  # compute mean transformed outcome (f applied to y) for both groups
  fee_p1 = mean(f(yz1, ...), na.rm = TRUE)
  fee_p0 = mean(f(yz0, ...), na.rm = TRUE)
  # compute counterfactual outcome transformation for untreated and treated groups
  fee1 = (((1 - p0) * fee_p1 - (1 - p1) * fee_p0) / (p1 - p0))
  fee0 = ((p1 * fee_p0 - p0 * fee_p1) / (p1 - p0))
  # compute difference in the inverse transformations of both counterfactuals
  eete = (f_inv(fee1, ...) - f_inv(fee0, ...))

  #INSERT VARIANCE HERE

  var_eete = NULL

  return(c(eete, var_eete))
}

#' Estimate the Egalitarian Equivalent Treatment Effect (eete) with Instrumental Variables
#'
#' This function estimates the eete, which optional arguments to include an IV or bootstrapped SE.
#' It applies an input function to data, conditioning on treatment (and instrument variables, if applicable).
#'
#' @param inputFunction A function that will be applied to the data.
#' @param ... Additional arguments to be passed to `inputFunction`.
#' @param y The name of the outcome variable column in `data`.
#' @param d The name of the treatment variable column in `data`.
#' @param data The data frame containing the variables.
#' @param f_inv Inverse function of f
#' @return A list containing the estimated egalitarian equivalent treatment effect and the estimated variance.
#' @import dplyr
#' @importFrom boot boot
#'
#' @export

non_iv_eete = function(f, ..., y, d, data, indices, f_inv, f_inv_prime){
  # create the option of using a bootstrap
  bdata = data[indices,]
  if (!all(bdata[[d]] %in% c(0, 1))) {
    stop("treatment indicator must only take values 0 (treatment) or 1 (control)")
  }
  # filter data based on treatment
  data1 = bdata %>%
    dplyr::filter(!!sym(d) == 1)
  data0 = bdata %>%
    dplyr::filter(!!sym(d) == 0)
  # extract outcome variables for both groups
  yz1 = data1[[y]]
  yz0 = data0[[y]]
  # mean transformed outcome for both groups
  fee1 = mean(f(yz1, ...), na.rm = TRUE)
  fee0 = mean(f(yz0, ...), na.rm = TRUE)
  # compute eete
  eete = (f_inv(fee1, ...) - f_inv(fee0, ...))
  # compute variance
  var_fee1 = var(f(yz1, ...), na.rm = TRUE)/nrow(data1)
  var_fee0 = var(f(yz0, ...), na.rm = TRUE)/nrow(data0)

  var_eete = (f_inv_prime(fee1, ...))^2 * var_fee1 + (f_inv_prime(fee0, ...))^2 * var_fee0

  return(c(eete, var_eete))
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
#' @param z Optional argument to include the name of the instrument variable column in `data`. z must only take values 0 or 1
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

eete = function(f, ..., y, d, z = NULL, data, lower = 0.1, upper = 100, se = NULL, B = 1000, f_inv = NULL) {

  # store the target column of the data at the data_test variable
  data_test = data %>%
    dplyr::select(!!sym(y))
  # handle a non numeric value returned by f
  if (is.character(f(data_test, ...))) {
    return(f(data_test, ...))
  }
  # checking if the function specified is one of the built in eete functions
  if (any(identical(f, eete::crpie), identical(f, eete::cdpie), identical(f, eete::cpiee))) {
    # if f_inv is specified, change to null so this can be handled by the package
    if (!is.null(f_inv)) {
      f_inv = NULL
    }
    # message for when lower and upper bounds are specified for built in functions
    if (lower != 0.1 | upper != 100){
      message("lower and upper parameters are options for custom functions, which require bootstrapped standard errors and uniroot inverses.
                These parameters will not affect eete::crpie, eete::cdpie, or eete::cpiee functions.")
    }
  }
  if (is.null(f_inv)) {
    # assign the inverse and inverse prime functions
    if (identical(f, eete::crpie)) {
      f_inv = function(x, ...) eete::crpie_inv(x, ...)
      f_inv_prime = function(x, ...) eete::crpie_inv_prime(x, ...)
    }
    else if (identical(f, eete::cdpie)) {
      f_inv = function(x) eete::cdpie_inv(x, ...)
      f_inv_prime = function(x) eete::cdpie_inv_prime(x, ...)
    }
    else if (identical(f, eete::cpiee)) {
      f_inv = function(x) eete::cpiee_inv(x, ...)
      f_inv_prime = function(x) eete::cpiee_inv_prime(x, ...)
      # no closed form standard error for cpiee
      if (!is.null(se)){
        se = "boot"
        message("No theoretical SE for cpiee, using bootstrapped SE")
      }
    }
    else {
      f_inv = inverse_fun(function(x) f(x, ...), lower, upper)
      # potential for this to be changed to generalized inverse prime function
      f_inv_prime = function(x) x
      # no closed form standard error for custom functions
      if (!is.null(se)){
        se = "boot"
        message("No theoretical SE for custom function, using bootstrapped SE")
      }
    }
  }
  # case where instrumental variables are specified
  if (!is.null(z)) {
    ee <- function(data, indices) {
      iv_eete(
        f = f,
        ...,
        y = y,
        d = d,
        z = z,
        data = data,
        indices = indices,
        f_inv = f_inv,
      )
    }
  }
  # case where instrumental variables are not specified
  else {
    ee <- function(data, indices) {
      non_iv_eete(
        f = f,
        ...,
        y = y,
        d = d,
        data = data,
        indices = indices,
        f_inv = f_inv,
        f_inv_prime = f_inv_prime,
      )
    }
  }

  eete = ee(data, 1:nrow(data))
  # return eete if user doesn't request the standard error
  if (is.null(se)) {
    return(list(estimate = eete[1]))
  }
  # return theoretical se if user requests
  if (se == "theoretical") {
    # compute bootstrapped se if theoretical not possible
    if (is.null(eete[2])){
      boot_results <- boot::boot(data = data, statistic = ee, R = B)
      se = sd(boot_results$t[,1])
      message("No theoretical SE function exists. Using bootstrapped SE.")
      return(list(estimate = eete[1], se = se))
    }
    # return theoretical se with eete
    else {
      se = sqrt(eete[2])
      return(list(estimate = eete[1], se = se))
    }
  }
  # return bootstrapped standard error if user requests
  boot_results <- boot::boot(data = data, statistic = ee, R = B)
  se = sd(boot_results$t[, 1], na.rm = TRUE)
  return(list(estimate = eete[1], se = se))
}

