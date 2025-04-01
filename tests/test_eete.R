# testing the eete script 
library(testthat)
source("C:/Users/jonat/OneDrive/economics_research/eete/R/eete.R")

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