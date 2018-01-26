context("Sharp RDD Estimation")

# data
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)

# linear/quadratic/cubic regression

test_that("rddapp vs. rddtools model 1", {
  rddapp_model <- rddapp::rd_est(y ~ x, cutpoint = 0, se.type = "const", t.design = 'geq')
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0)
  
  # linear
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "const"))

  # estimate
  expect_equal(unname(rddapp_model$est)[1], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[1], rddtools_test[2, 2]) 

  # quadratic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 2)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "const"))

  # estimate
  expect_equal(unname(rddapp_model$est)[2], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[2], rddtools_test[2, 2]) 

  # cubic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 3)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "const"))

  # estimate
  expect_equal(unname(rddapp_model$est)[3], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[3], rddtools_test[2, 2]) 
})

test_that("rddapp vs. rddtools model 2", {
  rddapp_model <- rddapp::rd_est(y ~ x, cutpoint = 0, se.type = "HC1", t.design = 'geq')
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0)
  
  # linear
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC1"))

  # estimate
  expect_equal(unname(rddapp_model$est)[1], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[1], rddtools_test[2, 2]) 

  # quadratic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 2)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC1"))

  # estimate
  expect_equal(unname(rddapp_model$est)[2], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[2], rddtools_test[2, 2])

  # cubic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 3)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC1"))

  # estimate
  expect_equal(unname(rddapp_model$est)[3], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[3], rddtools_test[2, 2])
})

test_that("rddapp vs. rddtools model 3", {
  rddapp_model <- rddapp::rd_est(y ~ x, cutpoint = 0.1, se.type = "HC0", t.design = 'geq')
  data_model <- rddtools::rdd_data(x = x, y = y, cutpoint = 0.1)
  
  # linear
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC0"))

  # estimate
  expect_equal(unname(rddapp_model$est)[1], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[1], rddtools_test[2, 2]) 

  # quadratic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 2)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC0"))

  # estimate
  expect_equal(unname(rddapp_model$est)[2], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[2], rddtools_test[2, 2])

  # cubic
  rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate", order = 3)
  rddtools_test <- lmtest::coeftest(rddtools_model, 
    vcov. = sandwich::vcovHC(rddtools_model, "HC0"))

  # estimate
  expect_equal(unname(rddapp_model$est)[3], rddtools_test[2, 1])  
  # standard error
  expect_equal(unname(rddapp_model$se)[3], rddtools_test[2, 2])
})

# local linear regression

test_that("rddapp vs. rdd model 1", {
  rddapp_model <- rddapp::rd_est(y ~ x, cutpoint = 0, bw = "IK09", t.design = "geq")
  rdd_model <- rdd::RDestimate(y ~ x, cutpoint = 0)

  # estimate
  expect_equal(unname(rddapp_model$est)[4:6], unname(rdd_model$est)[1:3], tolerance = 1e-3) 
  # bandwidth
  expect_equal(unname(rddapp_model$bw)[4:6], unname(rdd_model$bw)[1:3], tolerance = 1e-2) 
  # standard error
  expect_equal(unname(rddapp_model$se)[4:6], unname(rdd_model$se)[1:3], tolerance = 1e-2)
  # observation
  expect_equal(unname(rddapp_model$obs)[4:6], unname(rdd_model$obs)[1:3], tolerance = 1e-2) 
})

test_that("rddapp vs. rdd model 2", {
  rddapp_model <- rddapp::rd_est(y ~ x, cutpoint = 0.1, bw = "IK09", t.design = "geq")
  rdd_model <- rdd::RDestimate(y ~ x, cutpoint = 0.1)

  # estimate
  expect_equal(unname(rddapp_model$est)[4:6], unname(rdd_model$est)[1:3], tolerance = 1e-4) 
  # bandwidth
  expect_equal(unname(rddapp_model$bw)[4:6], unname(rdd_model$bw)[1:3], tolerance = 1e-4) 
  # standard error
  expect_equal(unname(rddapp_model$se)[4:6], unname(rdd_model$se)[1:3], tolerance = 1e-4)
  # observation
  expect_equal(unname(rddapp_model$obs)[4:6], unname(rdd_model$obs)[1:3]) 
})

test_that("rddapp vs. rdd model 3", {
  rddapp_model <- rddapp::rd_est(y ~ x, bw = 0.8, t.design = "geq")
  rdd_model <- rdd::RDestimate(y ~ x, bw = 0.8)

  # estimate
  expect_equal(unname(rddapp_model$est)[4:6], unname(rdd_model$est)[1:3]) 
  # bandwidth
  expect_equal(unname(rddapp_model$bw)[4:6], unname(rdd_model$bw)[1:3]) 
  # standard error
  expect_equal(unname(rddapp_model$se)[4:6], unname(rdd_model$se)[1:3])
  # observation
  expect_equal(unname(rddapp_model$obs)[4:6], unname(rdd_model$obs)[1:3]) 
})
