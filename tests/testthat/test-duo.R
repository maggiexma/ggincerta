test_that("duo() returns correct structure and attributes", {
  v1 <- c(1, 2, 3)
  v2 <- c(10, 20, 30)
  res <- duo(v1, v2)

  expect_s3_class(res, "bivariate")
  expect_true(inherits(res, "list"))
  expect_length(res, length(v1))
  expect_named(res[[1]], c("v1", "v2"))
  expect_equal(res[[1]]$v1, 1)
  expect_equal(res[[1]]$v2, 10)
  expect_equal(attr(res, "vars"), c(quote(v1), quote(v2)))
})

test_that("duo_exceed() returns correct structure and attributes", {
  est <- c(5, 6)
  err <- c(0.5, 0.6)
  res <- duo_exceed(est, err)

  expect_s3_class(res, "exceed")
  expect_true(inherits(res, "list"))
  expect_length(res, length(est))
  expect_named(res[[1]], c("v1", "v2"))
  expect_equal(res[[2]]$v1, 6)
  expect_equal(res[[2]]$v2, 0.6)
  expect_equal(attr(res, "vars"), c(quote(est), quote(err)))
})
