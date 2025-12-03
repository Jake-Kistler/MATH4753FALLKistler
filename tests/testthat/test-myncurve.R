library(testthat)

test_that("myncurve returns mu", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_identical(myncurve(10, 5, 6)$mu, 10)
})

test_that("myncurve returns sigma", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_identical(myncurve(10, 5, 6)$sigma, 5)
})

test_that("myncurve returns correct prob", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_equal(myncurve(10, 5, 6)$prob, pnorm(6, 10, 5), tolerance = 1e-12)
})
