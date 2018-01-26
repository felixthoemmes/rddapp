context("Sharp MFRDD Estimation")

if (.Platform$OS.type == "windows") {
  path <- "C:/Academia/Cornell/QML/RDD/rddapp/proj/reference/mrd_est/stata"
} else {
  path <- "~/rddapp/proj/reference/mrd_est/stata"
}

#### Using Wong et al. simulated data ####

if (file.exists(path)) {
  setwd(path)

  local_test1 <- file.exists("Curtis_seed100_m1_dat.csv") && file.exists("Curtis_seed100_m1_dat_h.csv")
  
  if (local_test1) {
    test_that("constant treatment effects (model 1)", {
      stata_dat <- read.csv("Curtis_seed100_m1_dat.csv")
      stata_dat_h <- read.csv("Curtis_seed100_m1_dat_h.csv")
  
      dat <- foreign::read.dta("Curtis_seed100_m1.dta")
      dat <- dat[c("readav", "mathav", "m_posty")]
      names(dat) <- c("x1", "x2", "y")
  
      mod <- rddapp::mfrd_est(y = dat$y, x1 = dat$x1, x2 = dat$x2, c1 = 40, c2 = 60)  
  
      ## compare datasets from mfrd_est with Stata
  
      expect_equal(stata_dat, mod$dat[names(stata_dat)], tolerance = 4e-8)
  
      expect_equal(stata_dat_h, mod$dat_h[names(stata_dat_h)], tolerance = 1e-7)
  
      # compare estimates from mfrd_est with Stata
  
      # STATA OUTPUT FOR constant treatment effects (model 1) 
      # ***EFFECT ESTIMATES FOR MULTIVARIATE I STANDARDIZED***
      # 
      # Math weight (st): .29635
      # Reading weight (st): .09788
      # 
      # Math frontier for complete model (st): 3.8443
      # Reading frontier for complete model (st): 4.018
      # ATE for complete model (st): 3.8874
      # 
      # Math frontier for heterogeneous treatment model (st): 3.9373
      # Reading frontier for heterogeneous treatment model (st): 3.9999
      # ATE for heterogeneous treatment model (st): 3.9529
      # 
      # Math frontier for treatment only model (st): 3.9562
      # Reading frontier for treatment only model (st): 3.9562
      # ATE for treatment only model (st): 3.9562
  
      # stata_est <- list(
      #   w1 = .09788,
      #   w2 = .29635,
      #   ev1 = 4.018,
      #   ev2 = 3.8443,
      #   ate = 3.8874,
      #   htev1 = 3.9999,
      #   htev2 = 3.9373,
      #   htate = 3.9529,
      #   tev1 = 3.9562,
      #   tev2 = 3.9562,
      #   tate = 3.9562)
  
      stata_w <- c(.09788, .29635)
      stata_est <- c(4.018, 3.8443, 3.8874, 
                     3.9999, 3.9373, 3.9529,
                     3.9562, 3.9562, 3.9562)
      
      expect_equal(stata_w, unname(mod$w), tolerance = 2e-4)
      expect_equal(stata_est, unname(mod$est), tolerance = 2e-4)
    })
  }
  
  local_test2 <- file.exists("Curtis_seed100_m2_dat.csv") && file.exists("Curtis_seed100_m2_dat_h.csv")
  
  if (local_test2) {
    test_that("constant treatment effects (model 2)", {
      stata_dat <- read.csv("Curtis_seed100_m2_dat.csv")
      stata_dat_h <- read.csv("Curtis_seed100_m2_dat_h.csv")
  
      dat <- foreign::read.dta("Curtis_seed100_m2.dta")
      dat <- dat[c("readav", "mathav", "m_posty")]
      names(dat) <- c("x1", "x2", "y")
  
      mod <- rddapp::mfrd_est(y = dat$y, x1 = dat$x1, x2 = dat$x2, c1 = 40, c2 = 60)  
  
      ## compare datasets from mfrd_est with Stata
  
      expect_equal(stata_dat, mod$dat[names(stata_dat)], tolerance = 4e-8)
  
      expect_equal(stata_dat_h, mod$dat_h[names(stata_dat_h)], tolerance = 2e-7)
  
      # compare estimates from mfrd_est with Stata
  
      # ***EFFECT ESTIMATES FOR MULTIVARIATE I STANDARDIZED***
      # 
      # Math weight (st): .29635
      # Reading weight (st): .09788
      # 
      # Math frontier for complete model (st): 5.8436
      # Reading frontier for complete model (st): 2.018
      # ATE for complete model (st): 4.8938
      # 
      # Math frontier for heterogeneous treatment model (st): 5.9366
      # Reading frontier for heterogeneous treatment model (st): 1.9999
      # ATE for heterogeneous treatment model (st): 4.9592
      # 
      # Math frontier for treatment only model (st): 4.9228
      # Reading frontier for treatment only model (st): 4.9228
      # ATE for treatment only model (st): 4.9228
  
      # stata_est <- list(
      #   w1 = .09788,
      #   w2 = .29635,
      #   ev1 = 2.018,
      #   ev2 = 5.8436,
      #   ate = 4.8938,
      #   htev1 = 1.9999,
      #   htev2 = 5.9366,
      #   htate = 4.9592,
      #   tev1 = 4.9228,
      #   tev2 = 4.9228,
      #   tate = 4.9228)
  
      stata_w <- c(.09788, .29635)
      stata_est <- c(2.018, 5.8436, 4.8938, 
                     1.9999, 5.9366, 4.9592,
                     4.9228, 4.9228, 4.9228)
      
      expect_equal(stata_w, unname(mod$w), tolerance = 6e-5)
      expect_equal(stata_est, unname(mod$est), tolerance = 6e-5)
    })
  }
  
  local_test3 <- file.exists("Curtis_seed100_m3_dat.csv") && file.exists("Curtis_seed100_m3_dat_h.csv")
  
  if (local_test3) {
    test_that("constant treatment effects (model 3)", {
      stata_dat <- read.csv("Curtis_seed100_m3_dat.csv")
      stata_dat_h <- read.csv("Curtis_seed100_m3_dat_h.csv")
  
      dat <- foreign::read.dta("Curtis_seed100_m3.dta")
      dat <- dat[c("readav", "mathav", "m_posty")]
      names(dat) <- c("x1", "x2", "y")
  
      mod <- rddapp::mfrd_est(y = dat$y, x1 = dat$x1, x2 = dat$x2, c1 = 40, c2 = 60)  
  
      ## compare datasets from mfrd_est with Stata
  
      expect_equal(stata_dat, mod$dat[names(stata_dat)], tolerance = 4e-8)
  
      expect_equal(stata_dat_h, mod$dat_h[names(stata_dat_h)], tolerance = 2e-7)
  
      # compare estimates from mfrd_est with Stata
  
      # ***EFFECT ESTIMATES FOR MULTIVARIATE I STANDARDIZED***
      # 
      # Math weight (st): .29635
      # Reading weight (st): .09788
      # 
      # Math frontier for complete model (st): 8.5018
      # Reading frontier for complete model (st): 2.018
      # ATE for complete model (st): 6.892
      # 
      # Math frontier for heterogeneous treatment model (st): 8.6423
      # Reading frontier for heterogeneous treatment model (st): 4.4313
      # ATE for heterogeneous treatment model (st): 7.5968
      # 
      # Math frontier for treatment only model (st): 7.5848
      # Reading frontier for treatment only model (st): 7.5848
      # ATE for treatment only model (st): 7.5848
  
      # stata_est <- list(
      #   w1 = .09788,
      #   w2 = .29635,
      #   ev1 = 2.018,
      #   ev2 = 8.5018,
      #   ate = 6.892,
      #   htev1 = 4.4313,
      #   htev2 = 8.6423,
      #   htate = 7.5968,
      #   tev1 = 7.5848,
      #   tev2 = 7.5848,
      #   tate = 7.5848)
  
      stata_w <- c(.09788, .29635)
      stata_est <- c(2.018, 8.5018, 6.892, 
                     4.4313, 8.6423, 7.5968,
                     7.5848, 7.5848, 7.5848)
      
      expect_equal(stata_w, unname(mod$w), tolerance = 6e-5)
      expect_equal(stata_est, unname(mod$est), tolerance = 6e-5)
    })
  }
}

