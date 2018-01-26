context("IK12 Optimal Bandwidth")

# data
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)

# IK 2012

test_that("rddapp vs. rddtools model 1", {
  rddapp_bw <- rddapp:::bw_ik12(X = x, Y = y, cutpoint = 0, kernel = "triangular")
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0)
  rddtools_bw <- rddtools::rdd_bw_ik(data_model, kernel = "Triangular") 

  # bandwidth
  expect_equal(rddapp_bw, unname(rddtools_bw), tolerance = 1e-4)  
})

test_that("rddapp vs. rddtools model 2", {
  rddapp_bw <- rddapp:::bw_ik12(X = x, Y = y, cutpoint = 0, kernel = "rectangular")
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0)
  rddtools_bw <- rddtools::rdd_bw_ik(data_model, kernel = "Uniform") 

  # bandwidth
  expect_equal(rddapp_bw, unname(rddtools_bw), tolerance = 1e-6)
})

test_that("rddapp vs. rddtools model 3", {
  rddapp_bw <- rddapp:::bw_ik12(X = x, Y = y, cutpoint = 0.1, kernel = "triangular")
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0.1)
  rddtools_bw <- rddtools::rdd_bw_ik(data_model, kernel = "Triangular") 

  # bandwidth
  expect_equal(rddapp_bw, unname(rddtools_bw), tolerance = 1e-4)
})
