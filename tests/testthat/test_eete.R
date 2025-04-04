# testing the eete script
library(testthat)
library(dplyr)
# importing the eete script, and the scripts for the evaluation functions for testing
# source("C:/Users/jonat/OneDrive/economics_research/eete/R/eete.R")
# source("C:/Users/jonat/OneDrive/economics_research/eete/R/crpie.R")
# source("C:/Users/jonat/OneDrive/economics_research/eete/R/cdpie.R")

# testing the inverse function
test_that("inverse_fun correctly computes inverse values", {
  inv_sqrt <- inverse_fun(function(x) x^2, 0, 100)
  expect_equal(inv_sqrt(9), 3, tolerance = 1e-6)
})

test_that("inverse_fun returns correct values", {
  inv_linear <- inverse_fun(function(x) 3*x + 5, -10, 10)
  expect_equal(inv_linear(8), 1, tolerance = 1e-6)
  expect_equal(inv_linear(5), 0, tolerance = 1e-6)
})

test_that("inverse_fun errors when no solution exists", {
  inv_exp <- inverse_fun(function(x) exp(x), -1, 1)
  expect_error(inv_exp(-1))  # exp(x) is always positive
})

test_that("inverse_fun errors when given an invalid interval", {
  expect_error(inverse_fun(function(x) x^2, -1, -0.5)(4))
})

# testing eete and support functions

# Create a small synthetic dataset
set.seed(123)

sample_data <- tibble(
  mpg = rnorm(10, mean = 25, sd = 5),  # Outcome variable (fuel efficiency)
  vs = sample(0:1, 10, replace = TRUE),  # Treatment indicator (engine type)
  am = sample(0:1, 10, replace = TRUE)   # Instrumental variable (transmission type)
)

# do a simple manual calculation

# fee_p1 = mean(mpg for z==1) and fee_p0 = mean(mpg for z==0),
# and p1, p0 are the proportions of treated in each group.
data1 <- sample_data %>% filter(am == 1)
data0 <- sample_data %>% filter(am == 0)
p1 <- mean(data1$vs)  # proportion treated in instrument group 1
p0 <- mean(data0$vs)  # proportion treated in instrument group 0
fee_p1 <- mean(crpie(data1$mpg, gam = 1))
fee_p0 <- mean(crpie(data0$mpg, gam = 1))
# Compute counterfactual outcomes
fee1_expected <- (((1 - p0) * fee_p1 - (1 - p1) * fee_p0) / (p1 - p0))
fee0_expected <- ((p1 * fee_p0 - p0 * fee_p1) / (p1 - p0))
# compute eete
expected_eete <- crpie_inv(fee1_expected, gam = 1) - crpie_inv(fee0_expected, gam =1)

# Testing the instrumental variables eete function
test_that("iv_eete computes the correct eete for a manual example", {
  result <- iv_eete(f = crpie, gam = 1, y = "mpg", d = "vs", z = "am",
                    data = sample_data, indices = 1:nrow(sample_data),
                    f_inv = crpie_inv)
  # result[1] is the computed eete; here we compare it with our expected value
  expect_equal(result[1], expected_eete, tolerance = 1e-6)
})

test_that("iv_eete handles non binary instrument", {
  non_binary = sample_data
  non_binary[ ,"am"] = 2
  expect_error(iv_eete(f = crpie, gam = 0.5, y = "mpg", d = "vs", z = "am",
                       data = non_binary, indices = 1:nrow(sample_data), f_inv = crpie_inv),
               "instrumental variable z must only take values 0 or 1")
})

test_that("iv_eete handles case with no individuals in treatment group", {
  no_control = sample_data
  no_control[ , "vs"] = 1
  expect_error(iv_eete(f = crpie, gam = 0.5, y = "mpg", d = "vs", z = "am",
                   data = no_control, indices = 1:nrow(sample_data), f_inv = crpie_inv),
               "the treatment and control groups must each have at least one observation")
})

test_that("iv_eete handles case with non binary treatment indicator", {
  non_binary = sample_data
  non_binary[ , "vs"] = 2
  expect_error(iv_eete(f = crpie, gam = 0.5, y = "mpg", d = "vs", z = "am",
                       data = non_binary, indices = 1:nrow(sample_data), f_inv = crpie_inv),
               "treatment indicator must only take values 0 (treatment) or 1 (control)", fixed = TRUE)
})








