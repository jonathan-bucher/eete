# install.packages("testthat")
library(testthat)

source("C:/Users/jonat/OneDrive/economics_research/eete/R/cdpie.R")

x = c(1, 2, 3, 4, 5)

test_that("cdpie handels null values for a and L case", {
  expect_error(cdpie(x), "Please provide a value for a or L.")
})

test_that("cdpie handles a < 0 case", {
  expect_error(cdpie(x, a = -1), "Please use a > 0")
})

test_that("cdpie works with properly specified a parameter", {
  # Test with a = 1
  result <- cdpie(1, a = 1)
  expect_equal(result, -exp(-1 * 1))
})

test_that("cdpie handles L < 0 case", {
  expect_error(cdpie(x, L = -1), "Please use L > 0")
})

test_that("cdpie works with properly specified L parameter", {
  result <- cdpie(1, L = 2)
  expect_equal(result, -2^(-1/2))
})

test_that("cdpie handles both a and L parameters with user prompt", {
  expect_error(cdpie(x, a = 1, L = 1), "Please specify a value for only a or L, not both")
})

# testing cdpie inverse

test_that("cdpie_inv handles x > 0 case", {
  expect_error(cdpie_inv(x, a = -1), "All values of x must be less than 0")
})

x = c(-1 , -2, -3, -4, -5)

test_that("cdpie_inv handles null values for a and L", {
  expect_error(cdpie_inv(x), "Please provide a value for a or L.")
})

test_that("cdpie_inv handles specified a and L parameters with user prompt",{
  expect_error(cdpie_inv(x, a = 1, L = 1), "Please specify a value for only a or L, not both")
})

test_that("cpdie_inv handles a < 0 case", {
  expect_error(cdpie_inv(x, a = -1), "Please use a > 0")
})

test_that("cdpie_inv handles a properly", {
  a = 1
  expect_equal(cdpie_inv(x, a = 1), -log(-x)/a)
})

test_that("cdpie_inv handles L properly", {
  L = 1
  expect_equal(cdpie_inv(x, L = 1), -L*log2(-x))
})

# testing cdpie_inv_prime

test_that("cdpie_inv_prime handles null values for a and L", {
  expect_error(cdpie_inv_prime(x), "Please provide a value for a or L.")
})

test_that("cdpie_inv_prime handles null values for a and L", {
  expect_error(cdpie_inv_prime(x, a = 1, L = 1), "Please specify a value for only a or L, not both")
})

test_that("cdpie_inv_prime handles a properly", {
  a = 1
  expect_equal(cdpie_inv_prime(x, a = 1), -1/(a*x))
})

test_that("cdpie_inv_prime handles L properly", {
  L = 1
  expect_equal(cdpie_inv_prime(x, L = 1), -L/(log(2)*x))
})




