context("Attrition Check")

# library
# library(AER)
# library(sandwich)
# library(lmtest)
# library(Formula)
# library(rddapp)
# library(rdd)
# library(rddtools)

# data
x1 <- c(NA, 0, 1, NA, 2, NA)
x2 <- c(5, NA, 3, 3, NA, NA)
y <- c(NA, 0, NA, 2, 3, 4)
t <- c(NA, NA, 0, 0, 1, NA)

# test

test_that("one assignment var", {
  attres1 <- rddapp:::attr_check(x1, y, t)

  # all
  expect_equal(attres1[[1]], length(t))
  expect_equal(attres1[[2]], length(na.omit(t[t == 0])))
  expect_equal(attres1[[3]], length(na.omit(t[t == 1])))
  expect_equal(attres1[[4]], length(t) - length(na.omit(t)))
  expect_equal(attres1[[5]], length(y) - length(na.omit(y)))
  expect_equal(attres1[[6]], table(y, t, useNA = "always")[5,1])
  expect_equal(attres1[[7]], table(y, t, useNA = "always")[5,2])
  expect_equal(attres1[[8]], length(x1) - length(na.omit(x1)))
  expect_equal(attres1[[9]], table(x1, t, useNA = "always")[4,1])
  expect_equal(attres1[[10]], table(x1, t, useNA = "always")[4,2])
})

test_that("two assignment vars", {
  attres2 <- rddapp:::attr_check(x1,y,t,x2)

  # bandwidth
  expect_equal(attres2[[1]], length(t))
  expect_equal(attres2[[2]], length(na.omit(t[t == 0])))
  expect_equal(attres2[[3]], length(na.omit(t[t == 1])))
  expect_equal(attres2[[4]], length(t) - length(na.omit(t)))
  expect_equal(attres2[[5]], length(y) - length(na.omit(y)))
  expect_equal(attres2[[6]], table(y, t, useNA = "always")[5,1])
  expect_equal(attres2[[7]], table(y, t, useNA = "always")[5,2])
  expect_equal(attres2[[8]], length(x1) - length(na.omit(x1)))
  expect_equal(attres2[[9]], table(x1, t, useNA = "always")[4,1])
  expect_equal(attres2[[10]], table(x1, t, useNA = "always")[4,2])
  expect_equal(attres2[[11]], length(x2) - length(na.omit(x2)))
  expect_equal(attres2[[12]], table(x2, t, useNA = "always")[3,1])
  expect_equal(attres2[[13]], table(x2, t, useNA = "always")[3,2])
})
