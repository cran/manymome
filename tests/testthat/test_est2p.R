library(testthat)
library(manymome)

set.seed(59876)
n <- 5000
x1 <- rnorm(n, 2, 4)
x2 <- rnorm(n, 2, 1)
x3 <- rnorm(n, -2, 4)
x4 <- rnorm(n, -2, 1)
x5 <- rnorm(n, 0, 1)
xNA <- rep(NA, n)
x11 <- rnorm(99, 2, 1)

test_that("est2p", {
    expect_equal(est2p(x1), 0.6228)
    expect_equal(est2p(x2), 0.0432)
    expect_equal(est2p(x3), 0.6452)
    expect_equal(est2p(x4), 0.0456)
    expect_equal(est2p(x5), 0.9796)
    expect_equal(est2p(-x2), 0.0432)
    expect_equal(est2p(-x4), 0.0456)
    expect_equal(est2p(-x3), 0.6452)
    expect_equal(est2p(-x5), 0.9796)
    expect_equal(est2p(xNA), NA)
    expect_equal(suppressWarnings(est2p(x11)), NA)
    expect_warning(est2p(x11, warn = TRUE))
  })
