# install.packages("testthat")
library(testthat)

source("C:/Users/jonat/OneDrive/economics_research/eete/R/cpiee.R")

x = c(1, 2, 3, 4, 5)

test_that("cpiee handles x < c case", {
  expect_error(cpiee(x, eta = 1, c = 2), "All values of X must be greater than c")
})
test_that("cpiee handles eta < 0 case", {
  expect_error(cpiee(x, eta = -1, c = 0), "Please use eta >= 0")
})
# need test for the c = 0 case, in which case this test would fail
test_that("cpiee handles c == 0 case", {
  expect_error(cpiee(x, eta = 10, c = 0), "please use nonzero c value")
})
test_that("cpiee handles eta != 1 case", {
  eta = 10
  c = 0.5
  expect_equal(cpiee(x, eta, c), (log(abs(x) / c))^(1 - eta) / (1 - eta))
})
test_that("cpiee handles eta == 1 case", {
  eta = 1
  c = 0.5
  expect_equal(cpiee(x, eta, c), log(log(abs(x) / c)))
})
