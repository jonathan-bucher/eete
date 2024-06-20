#' Find the EETE Bounds
#'
#' This function returns a data frame of the upper and lower bounds calculations.
#'
#' @param inputFunction A function to apply to the outcome variable.
#' @param ... Additional arguments for the input function.
#' @param y A string indicating the outcome variable column name.
#' @param d A string indicating the selection or treatment variable column name.
#' @param z A string indicating the instrument variable column name.
#' @param wgt A vector of weights.
#' @param data A data frame containing y, d, and z columns.
#' @return A data frame with probabilities and bounds.
#' @export

ebounds = function(inputFunction, ..., y, d, z, wgt = NULL, data, se = FALSE, B = 1000){

  data_test = data |>
    dplyr::select(!!sym(y))

  if (is.character(inputFunction(data_test, ...))) {
    all_results = inputFunction(data_test, ...)

  } else {

    bounds = function(data, indices){

      bdata = data[indices,]

      bdata = bdata |>
        rename(y = !!sym(y),
               z = !!sym(z),
               s = !!sym(d)) |>
        mutate(wgt = ifelse(is.null(wgt), 1, wgt),
               y = inputFunction(y, ...)) |>
        filter(!is.na(z), s %in% c(0, 1), !(is.na(y) & s == 1)) |>
        mutate(nn = as.integer(z == 1 & s == 0), # treated and unemployed
               ee = as.integer(z == 0 & s == 1), # untreated and employed
               nn_ne = as.integer(z == 0 & s == 0), # untreated and unemployed
               ee_ne = as.integer(z == 1 & s == 1)) # treated and employed

      props = bdata |>
        summarise(pr_nn = sum(nn * wgt, na.rm = TRUE) / sum(wgt, na.rm = TRUE), # the proportion of unemployed in the treated group
                  pr_ee = sum(ee * wgt, na.rm = TRUE) / sum(wgt, na.rm = TRUE), # the proportion of employed in the untreated group
                  pr_nn_ne = sum(nn_ne * wgt, na.rm = TRUE) / sum(wgt, na.rm = TRUE), # the proportion of unemployed in the untreated group
                  pr_ee_ne = sum(ee_ne * wgt, na.rm = TRUE) / sum(wgt, na.rm = TRUE)) |> # the proportion of employed in the treated group
        mutate(pr_ne = pr_ee_ne - pr_ee,
               pr_ne2 = pr_nn_ne - pr_nn,
               tprop = (pr_ee / pr_ee_ne) * 100,
               one_tprop = 100 - tprop)

      means = bdata |>
        summarise(y_ee0 = weighted.mean(y[ee == 1], w = wgt[ee == 1], na.rm = TRUE), # the mean y if employed and untreated
                  y_11 = weighted.mean(y[ee_ne == 1], w = wgt[ee_ne == 1], na.rm = TRUE), # the mean y if employed and treated
                  lower_y = min(y, na.rm = TRUE),
                  upper_y = max(y, na.rm = TRUE))

      bdata = bdata |>
        filter(s == 1, z == 1) |>
        mutate(rankp2 = rank(y))


      p10ovp11 = props$tprop
      cp10ovp11 = props$one_tprop

      y11povp = quantile(bdata$rankp2, probs = p10ovp11 / 100, na.rm = TRUE)
      y11cpovp = quantile(bdata$rankp2, probs = cp10ovp11 / 100, na.rm = TRUE)

      y_ee1_lb = mean(bdata$y[bdata$rankp2 < y11povp], na.rm = TRUE)
      y_ee1_ub = mean(bdata$y[bdata$rankp2 > y11cpovp], na.rm = TRUE)
      Ya_l = props$pr_ee * means$y_ee0 + props$pr_nn * means$lower_y # the lower bound of outcome y for the untreated
      Ya_u = props$pr_ee * means$y_ee0 + props$pr_nn * means$upper_y # the upper bound of outcome y for the untreated
      Yb_l = props$pr_ee_ne * means$y_11 + props$pr_nn_ne * means$lower_y # the lower bound of outcome y for the treated
      Yb_u = props$pr_ee_ne * means$y_11 + props$pr_nn_ne * means$upper_y # the upper bound of outcome y for the untreated

      LB_ee_mono = y_ee1_lb - means$y_ee0
      UB_ee_mono = y_ee1_ub - means$y_ee0
      LB_ee_dom = means$y_11 - means$y_ee0
      UB_HM = Yb_u - Ya_l
      LB_HM = Yb_l - Ya_u

      return(c(Ya_l, means$y_ee0, Ya_u, Yb_l, y_ee1_lb, means$y_11, Yb_u, y_ee1_ub))
    }

    estimates = c(bounds(data, 1:nrow(data)))

    all_results = data.frame(Bounds = c("Horowitz and Manski", "Lee", "Chen and Flores"),
                             ee_L_a = c(estimates[1], estimates[2], estimates[2]),
                             ee_U_a = c(estimates[3], estimates[2], estimates[2]),
                             ee_L_b = c(estimates[4], estimates[5], estimates[6]),
                             ee_U_b = c(estimates[7], estimates[8], estimates[8]))

  if (se == TRUE) {

    boot_results = boot::boot(data, bounds, R = B)

    all_results$ee_L_a_SE = c(sd(boot_results$t[,1]),
                              sd(boot_results$t[,2]),
                              sd(boot_results$t[,2]))

    all_results$ee_U_a_SE = c(sd(boot_results$t[,3]),
                              sd(boot_results$t[,2]),
                              sd(boot_results$t[,2]))

    all_results$ee_L_b_SE = c(sd(boot_results$t[,4]),
                              sd(boot_results$t[,5]),
                              sd(boot_results$t[,6]))

    all_results$ee_U_b_SE = c(sd(boot_results$t[,7]),
                              sd(boot_results$t[,8]),
                              sd(boot_results$t[,8]))


  } else {

    all_results = all_results
  }
  }
  return(all_results)
}
