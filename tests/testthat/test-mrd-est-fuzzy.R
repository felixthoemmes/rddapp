context("Fuzzy MRDD Estimation")

# data
set.seed(12345)
x1 <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x1 + 3 * cov + 10 * (x2 >= 0) + rnorm(1000)
z <- rbinom(1000, 1, 0.5)

# centering

test_that("mrd vs. rd model 1", {
  cutoff <- c(0, 0)
  t.design <- c("l", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "center", 
    t.design = t.design)[["center"]]$tau_MRD
  x <- rddapp:::var_center(cbind(x1, x2), cut = cutoff, t.design = t.design)
  rd_model <- rddapp::rd_est(y ~ x + z, cutpoint = 0, t.design = "l")

  # estimate
  expect_equal(unname(mrd_model$est), unname(rd_model$est)) 
  # bandwidth
  expect_equal(unname(mrd_model$bw), unname(rd_model$bw)) 
  # standard error
  expect_equal(unname(mrd_model$se), unname(rd_model$se))
  # observation
  expect_equal(unname(mrd_model$obs), unname(rd_model$obs))
})

test_that("mrd vs. rd model 2", {
  cutoff <- c(0.1, -0.1)
  t.design <- c("l", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "center", 
    t.design = t.design)[["center"]]$tau_MRD
  x <- rddapp:::var_center(cbind(x1, x2), cut = cutoff, t.design = t.design)
  rd_model <- rddapp::rd_est(y ~ x + z, cutpoint = 0, t.design = "l")
  
  # estimate
  expect_equal(unname(mrd_model$est), unname(rd_model$est)) 
  # bandwidth
  expect_equal(unname(mrd_model$bw), unname(rd_model$bw)) 
  # standard error
  expect_equal(unname(mrd_model$se), unname(rd_model$se))
  # observation
  expect_equal(unname(mrd_model$obs), unname(rd_model$obs)) 
})

test_that("mrd vs. rd model 3", {
  cutoff <- c(0, 0)
  t.design <- c("g", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "center", 
    t.design = t.design)[["center"]]$tau_MRD
  x <- rddapp:::var_center(cbind(x1, x2), cut = cutoff, t.design = t.design)
  rd_model <- rddapp::rd_est(y ~ x + z, cutpoint = 0, t.design = "l")
  
  # estimate
  expect_equal(unname(mrd_model$est), unname(rd_model$est)) 
  # bandwidth
  expect_equal(unname(mrd_model$bw), unname(rd_model$bw)) 
  # standard error
  expect_equal(unname(mrd_model$se), unname(rd_model$se))
  # observation
  expect_equal(unname(mrd_model$obs), unname(rd_model$obs)) 
})

test_that("mrd vs. rd model 4", {
  cutoff <- c(-0.1, 0.1)
  t.design <- c("g", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "center", 
    t.design = t.design)[["center"]]$tau_MRD
  x <- rddapp:::var_center(cbind(x1, x2), cut = cutoff, t.design = t.design)
  rd_model <- rddapp::rd_est(y ~ x + z, cutpoint = 0, t.design = "l")
  
  # estimate
  expect_equal(unname(mrd_model$est), unname(rd_model$est)) 
  # bandwidth
  expect_equal(unname(mrd_model$bw), unname(rd_model$bw)) 
  # standard error
  expect_equal(unname(mrd_model$se), unname(rd_model$se))
  # observation
  expect_equal(unname(mrd_model$obs), unname(rd_model$obs))
})

# univariate

test_that("mrd vs. rd model 1", {
  cutoff <- c(0, 0)
  t.design <- c("l", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "univ", 
    t.design = t.design)[["univ"]]
  mrd_model1 <- mrd_model$tau_R
  mrd_model2 <- mrd_model$tau_M
  rd_model1 <- rddapp::rd_est(y ~ x1 + z, subset = (x2 >= cutoff[2]), cutpoint = cutoff[1],
    t.design = t.design[1])
  rd_model2 <- rddapp::rd_est(y ~ x2 + z, subset = (x1 >= cutoff[1]), cutpoint = cutoff[2],
    t.design = t.design[2])
  
  # estimate
  expect_equal(unname(mrd_model1$est), unname(rd_model1$est)) 
  expect_equal(unname(mrd_model2$est), unname(rd_model2$est))
  # bandwidth
  expect_equal(unname(mrd_model1$bw), unname(rd_model1$bw)) 
  expect_equal(unname(mrd_model2$bw), unname(rd_model2$bw)) 
  # standard error
  expect_equal(unname(mrd_model1$se), unname(rd_model1$se))
  expect_equal(unname(mrd_model2$se), unname(rd_model2$se))
  # observation
  expect_equal(unname(mrd_model1$obs), unname(rd_model1$obs))
  expect_equal(unname(mrd_model2$obs), unname(rd_model2$obs))
})

test_that("mrd vs. rd model 2", {
  cutoff <- c(0.1, -0.1)
  t.design <- c("l", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "univ", 
    t.design = t.design)[["univ"]]
  mrd_model1 <- mrd_model$tau_R
  mrd_model2 <- mrd_model$tau_M
  rd_model1 <- rddapp::rd_est(y ~ x1 + z, subset = (x2 >= cutoff[2]), cutpoint = cutoff[1],
    t.design = t.design[1])
  rd_model2 <- rddapp::rd_est(y ~ x2 + z, subset = (x1 >= cutoff[1]), cutpoint = cutoff[2],
    t.design = t.design[2])
  
  # estimate
  expect_equal(unname(mrd_model1$est), unname(rd_model1$est)) 
  expect_equal(unname(mrd_model2$est), unname(rd_model2$est))
  # bandwidth
  expect_equal(unname(mrd_model1$bw), unname(rd_model1$bw)) 
  expect_equal(unname(mrd_model2$bw), unname(rd_model2$bw)) 
  # standard error
  expect_equal(unname(mrd_model1$se), unname(rd_model1$se))
  expect_equal(unname(mrd_model2$se), unname(rd_model2$se))
  # observation
  expect_equal(unname(mrd_model1$obs), unname(rd_model1$obs))
  expect_equal(unname(mrd_model2$obs), unname(rd_model2$obs))
})

test_that("mrd vs. rd model 3", {
  cutoff <- c(0, 0)
  t.design <- c("g", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "univ", 
    t.design = t.design)[["univ"]]
  mrd_model1 <- mrd_model$tau_R
  mrd_model2 <- mrd_model$tau_M
  rd_model1 <- rddapp::rd_est(y ~ x1 + z, subset = (x2 >= cutoff[2]), cutpoint = cutoff[1],
    t.design = t.design[1])
  rd_model2 <- rddapp::rd_est(y ~ x2 + z, subset = (x1 <= cutoff[1]), cutpoint = cutoff[2],
    t.design = t.design[2])
  
  # estimate
  expect_equal(unname(mrd_model1$est), unname(rd_model1$est)) 
  expect_equal(unname(mrd_model2$est), unname(rd_model2$est))
  # bandwidth
  expect_equal(unname(mrd_model1$bw), unname(rd_model1$bw)) 
  expect_equal(unname(mrd_model2$bw), unname(rd_model2$bw)) 
  # standard error
  expect_equal(unname(mrd_model1$se), unname(rd_model1$se))
  expect_equal(unname(mrd_model2$se), unname(rd_model2$se))
  # observation
  expect_equal(unname(mrd_model1$obs), unname(rd_model1$obs))
  expect_equal(unname(mrd_model2$obs), unname(rd_model2$obs))
})

test_that("mrd vs. rd model 4", {
  cutoff <- c(-0.1, 0.1)
  t.design <- c("g", "l")
  mrd_model <- rddapp::mrd_est(y ~ x1 + x2 + z, cutpoint = cutoff, method = "univ", 
    t.design = t.design)[["univ"]]
  mrd_model1 <- mrd_model$tau_R
  mrd_model2 <- mrd_model$tau_M
  rd_model1 <- rddapp::rd_est(y ~ x1 + z, subset = (x2 >= cutoff[2]), cutpoint = cutoff[1],
    t.design = t.design[1])
  rd_model2 <- rddapp::rd_est(y ~ x2 + z, subset = (x1 <= cutoff[1]), cutpoint = cutoff[2],
    t.design = t.design[2])
  
  # estimate
  expect_equal(unname(mrd_model1$est), unname(rd_model1$est)) 
  expect_equal(unname(mrd_model2$est), unname(rd_model2$est))
  # bandwidth
  expect_equal(unname(mrd_model1$bw), unname(rd_model1$bw)) 
  expect_equal(unname(mrd_model2$bw), unname(rd_model2$bw)) 
  # standard error
  expect_equal(unname(mrd_model1$se), unname(rd_model1$se))
  expect_equal(unname(mrd_model2$se), unname(rd_model2$se))
  # observation
  expect_equal(unname(mrd_model1$obs), unname(rd_model1$obs))
  expect_equal(unname(mrd_model2$obs), unname(rd_model2$obs))
})