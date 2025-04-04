# testing for the crpie function
library(testthat)

x = c(-1, 0, 0.5, 2, 3)
x_pos = c(1, 0.1, 0.5, 2, 3)

# beginning testing for the cdpie function

test_that("crpie handles unspecified parameters", {
  expect_error(crpie(x), "Please provide a value for gamma or k.")
})

test_that("crpie handles both gamma and k specified", {
  expect_error(crpie(x, gam = 1, k = 0), "Please do not specify both gamma and k.")
})

test_that("crpie hangles negative gamma case", {
  expect_error(crpie(x, gam = -1), "Values for gamma must be positive.")
})

test_that("crpie handles gamma == 1 with negative values in x vector", {
  expect_error(crpie(x, gam = 1), "for cases where gamma is specified to be a value other than one, all values of x must be positive", fixed = TRUE)
})

test_that("crpie handles gamma != 1 case", {
  gam = 0.5
  expect_equal(crpie(x, gam = 0.5), x^(1-gam)/(1-gam))
})

test_that("crpie handles gamma == 1 case", {
  expect_equal(crpie(x = x_pos, gam = 1), log(x_pos))
})

test_that("crpie handles specified k with negative x values", {
  expect_error(crpie(x, k = 0), "for cases where k is specified as 0, all values of x must be positive")
})

test_that("crpie handles k == 0 case", {
  expect_equal(crpie(x = x_pos, k = 0), log(x_pos))
})

test_that("crpie handles negative k", {
  expect_error(crpie(x, k = -1), "k must be positive")
})

test_that("crpie handles 1 <= k < 2 case", {
  expect_error(crpie(x, k = 1), "Values for k between 1 (inclusive) and 2 (exclusive) are not allowed.", fixed = TRUE)
})

test_that("crpie handles k not equal to zero, and not between 1 (inclusive) and 2 (exclusive)", {
  k = 0.5
  expect_equal(crpie(x, k = 0.5), log2(k) * x^(1/(log2(k))))
})

# beginning testing for crpie_inv function
test_that("crpie_inv handles no specified parameters", {
  expect_error(crpie_inv(x), "Please provide a value for gamma or k.")
})

test_that("crpie_inv handles two specified parameters", {
  expect_error(crpie_inv(x, gam = 1, k = 3), "Please do not specify both gamma and k")
})

test_that("crpie_inv handles negative gamma values", {
  expect_error(crpie_inv(x, gam = -1), "Values for gamma must be positive.")
})

test_that("crpie_inv handles gamma != 1", {
  gam = 0.5
  expect_equal(crpie_inv(x, gam = 0.5), (x * (1 - gam)) ^ (1 / (1-gam)))
})

test_that("crpie_inv handles gamma == 1", {
  expect_equal(crpie_inv(x, gam = 1), exp(x))
})

# testing for crpie_inv_prime function
test_that("crpie_inv_prime handles neither gamma or k specified", {
  expect_error(crpie_inv_prime(x), "Please provide a value for gamma or k.")
})

test_that("crpie_inv_prime handles both gamma and k specified", {
  expect_error(crpie_inv_prime(x, gam = 1, k = 1), "Please do not provide a value for both gamma and k")
})

test_that("crpie_inv_prime handles negative gamma", {
  expect_error(crpie_inv_prime(x, gam = -1), "Values for gamma must be positive.")
})

test_that("crpie_inv_prime handles negative gamma", {
  expect_error(crpie_inv_prime(x, gam = -1), "Values for gamma must be positive.")
})

test_that("crpie_inv_prime handles gamma != 1", {
  gam = 0.5
  expect_equal(crpie_inv_prime(x, gam = 0.5), (x*(1-gam))^(gam/(1-gam)))
})

test_that("crpie_inv_prime handles gamma == 1", {
  expect_equal(crpie_inv_prime(x, gam = 1), exp(x))
})

test_that("crpie_inv_prime handles negative k", {
  expect_error(crpie_inv_prime(x, k = -1), "k must be positive")
})

test_that("crpie handles k == 0", {
  expect_equal(crpie_inv_prime(x, k =0), exp(x))
})

test_that("crpie handles 1 <= k < 2", {
  expect_error(crpie_inv_prime(x, k = 1.5), "Values for k between 1 (inclusive) and 2 (exclusive) are not allowed", fixed = TRUE)
})

test_that("crpie handles positive k not between one and two", {
  k = 10
  expect_equal(crpie_inv_prime(x, k = 10), (x/(log2(k)))^(log2(k) - 1))
})


