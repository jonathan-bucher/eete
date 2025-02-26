# install.packages("testthat")
library(testthat)

source("C:/Users/jonat/OneDrive/economics_research/eete/R/cdpie.R")

x = c(1, 2, 3, 4, 5)

cdpie(x, L = 1)

test_that("null values for a and L properly handled in cdpie()", {

  result = cdpie(x)
  expect_equal(result, "Please provide a value for a or L.")

})

test_that("cdpie works with a parameter", {
  # Test with a = 1
  result <- cdpie(1, a = 1)
  expect_equal(result, -exp(-1 * 1))  # should be approximately -0.3678794

  # Test with a = 0 (expect a message about using a > 0)
  result <- cdpie(1, a = 0)
  expect_equal(result, "Please use a > 0")

  # Test with missing a and L (expect message to provide a value for a or L)
  result <- cdpie(1)
  expect_equal(result, "Please provide a value for a or L.")
})

# Test 2: Test with 'L' parameter
test_that("cdpie works with L parameter", {
  # Test with L = 2
  result <- cdpie(1, L = 2)
  expect_equal(result, -2^(-1/2))  # should be approximately -0.7071068

  # Test with L = 0 (expect message about using L > 0)
  result <- cdpie(1, L = 0)
  expect_equal(result, "Please use L > 0")
})

# Test 3: Test with missing a and L
test_that("cdpie handles missing a and L", {
  result <- cdpie(1)
  expect_equal(result, "Please provide a value for a or L.")
})

# Test 4: Test with both a and L
test_that("cdpie handles both a and L parameters", {
  result <- cdpie(1, a = 1, L = 2)
  expect_equal(result, -exp(-1 * 1))  # Only 'a' is used, L is ignored
})

# Test 5: Test edge case with x = 0
test_that("cdpie handles x = 0", {
  # For a > 0
  result <- cdpie(0, a = 1)
  expect_equal(result, -exp(-1 * 0))  # should be -1

  # For L > 0
  result <- cdpie(0, L = 2)
  expect_equal(result, -2^(0/2))  # should be -1
})

# Test 6: Test for invalid negative values of a and L (potential improvements)
test_that("cdpie handles negative values for a and L", {
  result <- cdpie(1, a = -1)
  expect_equal(result, -exp(1))  # it should handle negative a, which leads to exp(positive value)

  result <- cdpie(1, L = -2)
  expect_equal(result, -2^(-1/(-2)))  # should handle negative L correctly
})

# testing cdpie inverse
# manual verification shows the inverse function is being properly calculated for a and L


test_that("cdpie_inv handles null values for a and L", {
  result = cdpie_inv(x)
  expect_equal(result, "Please provide a value for a or L.")
})

test_that("cdpie_inv handles positive value for a with unspecified L", {
  result = cdpie_inv(x, a = 1)
  expect_equal(result, )
})

