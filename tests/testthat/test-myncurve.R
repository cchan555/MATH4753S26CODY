test_that("mu correct", {
  result <- Lab6_myncurve(10, 5, 6)
  expect_equal(result$mu, 10)
})

test_that("sigma correct", {
  result <- Lab6_myncurve(10, 5, 6)
  expect_equal(result$sigma, 5)
})

test_that("a correct", {
  result <- Lab6_myncurve(10, 5, 6)
  expect_equal(result$a, 6)
})
