# testing the eete script 
library(testthat)
library(dplyr)
source("C:/Users/jonat/OneDrive/economics_research/eete/R/eete.R")
source("C:/Users/jonat/OneDrive/economics_research/eete/R/crpie.R")

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

# testing the iv_eete function
test_that("iv_eete handles non binary instrument", {
  sample_data1 = sample_data
  sample_data1[ ,"am"] = 2
  expect_error(iv_eete(f = crpie, gam = 0.5, y = "mpg", d = "vs", z = "am",
                       data = sample_data1, indices = 1:nrow(sample_data), f_inv = crpie_inv), 
               "instrumental variable z must only take values 0 or 1")
})







