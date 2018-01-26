context("IK09 Optimal Bandwidth")

if (.Platform$OS.type == "windows") {
  path <- "C:/Academia/Cornell/QML/RDD/rddapp/proj/reference/bw_ik"
} else {
  path <- "~/rddapp/proj/reference/bw_ik"
}

# IK 2009

if (file.exists(path)) {
  setwd(path)
  
  local_test <- file.exists("art_sharp_rd.dat") && file.exists("art_fuzzy_rd.dat")
  
  if (local_test) {
    # data

    art_sharp_rd <- read.table("art_sharp_rd.dat", col.names = c("y", "x", "z1", "z2", "z3", "z4"))
    art_fuzzy_rd <- read.table("art_fuzzy_rd.dat", col.names = c("y", "w", "x", "z1", "z2", "z3"))
  
    # STATA CODE FROM rd_stata_09aug4.do
    # /* estimate rd effect */
    # /* y is outcome */
    # /* x is forcing variable */
    # /* z1, z2, z3 are additional covariates */
    # /* w is treatment indicator */
    # /* c(0.5) implies that threshold is 0.5 */
    
    # sharp rdd
    
    test_that("cutpoint 0", {
      # STATA COMMAND:
      # . rdob y x z1 z2 z3 z4, c(0)
      # STATA OUTPUT:
      # optimal bandwidth(h_opt) = .85903018
      # RD point estimate = -.13594639
      # RD standard error = .05370893
      
      rddapp_bw <- with(art_sharp_rd, rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0))
      
      # bandwidth
      expect_equal(rddapp_bw, .85903018, tolerance = 8e-4)
    })
    
    test_that("cutpoint 0.299", {
      # STATA COMMAND:
      # . rdob y x z1 z2 z3 z4, c(0.2990)
      # STATA OUTPUT:
      # optimal bandwidth(h_opt) = .67002881
      # RD point estimate = .99959946
      # RD standard error = .05290336
      
      rddapp_bw <- with(art_sharp_rd, rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0.299))
      
      # bandwidth
      expect_equal(rddapp_bw, .67002881, tolerance = 4e-2) 
    })
    
    # fuzzy rdd
    
    test_that("cutpoint 0", {
      # STATA COMMAND: 
      # rdob y x z1 z2 z3, c(0)  fuzzy(w)
      #
      # STATA OUTPUT:
      # optimal bandwidth(h_opt) = .51018551
      # RD point estimate = 6.5999966
      # RD standard error = 25.12072
      
      rddapp_bw <- with(art_fuzzy_rd, rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0))
      
      # bandwidth
      expect_equal(rddapp_bw, .51018551, tolerance = 2e-3)
    })
    
    test_that("cutpoint 0.5", {
      # STATA COMMAND:
      # rdob y x z1 z2 z3, c(0.5)  fuzzy(w)
      #
      # STATA OUTPUT:
      # optimal bandwidth(h_opt) = .84015499
      # RD point estimate = 1.0622666
      # RD standard error = .06672707
      
      rddapp_bw <- with(art_fuzzy_rd, rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0.5))
      
      # bandwidth
      expect_equal(rddapp_bw, .84015499, tolerance = 4e-3)  
    })
  }
}

# data
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)

test_that("rddapp vs. rdd model 1", {
  rddapp_bw <- rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0, kernel = "triangular")
  rdd_bw <- rdd::IKbandwidth(X = x, Y = y, cutpoint = 0, kernel = "triangular")

  # bandwidth
  expect_equal(rddapp_bw, rdd_bw, tolerance = 1e-2) 
})

test_that("rddapp vs. rdd model 2", {
  rddapp_bw <- rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0, kernel = "epanechnikov")
  rdd_bw <- rdd::IKbandwidth(X = x, Y = y, cutpoint = 0, kernel = "epanechnikov")

  # bandwidth
  expect_equal(rddapp_bw, rdd_bw, tolerance = 1e-2) 
})

test_that("rddapp vs. rdd model 3", {
  rddapp_bw <- rddapp:::bw_ik09(X = x, Y = y, cutpoint = 0.1, kernel = "triangular")
  rdd_bw <- rdd::IKbandwidth(X = x, Y = y, cutpoint = 0.1, kernel = "triangular")

  # bandwidth
  expect_equal(rddapp_bw, rdd_bw, tolerance = 1e-4)  
})