context("Sharp RDD Prediction")

# data
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
cut <- 0

for (t.design in c("g", "geq", "l", "leq")) {
  # sharp rdd

  tr <- rddapp:::treat_assign(x, cut, t.design)
  y <- 3 + 2 * x + 3 * cov + 10 * tr + rnorm(1000)
  dat <- data.frame(x, y, cov, tr)
  
  test_that("sharp lm vs. fuzzy ivreg", {
    # estimate sharp by sharp rd_est using lm
    rd_sharp <- rddapp::rd_est(y ~ x | cov, data = dat, cutpoint = cut, t.design = t.design) 
    
    # estimate sharp by fuzzy rd_est using ivreg
    rd_fuzzy <- rddapp::rd_est(y ~ x + tr | cov, data = dat, cutpoint = cut, t.design = t.design)
    
    # predict sharp by predict.rd
    pred_fuzzy <- predict(rd_fuzzy)
    
    # new data
    newdat <- pred_fuzzy[c("X", "Tr", "cov_gm")]
    
    newdat$Xl <- (newdat$X - cut) * (1 - newdat$Tr)
    newdat$Xr <- (newdat$X - cut) * newdat$Tr
    
    newdat$cov <- newdat$cov_gm
    
    ## predict sharp by predict.lm
    
    # linear
    pred_sharp <- predict(rd_sharp$model[[1]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.linear", unname(pred_sharp$fit))
    expect_equal(pred_fuzzy$"YSE.linear", unname(pred_sharp$se.fit))
    
    # quadratic
    pred_sharp <- predict(rd_sharp$model[[2]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.quadratic", unname(pred_sharp$fit))
    expect_equal(pred_fuzzy$"YSE.quadratic", unname(pred_sharp$se.fit))
    
    # cubic
    pred_sharp <- predict(rd_sharp$model[[3]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.cubic", unname(pred_sharp$fit))
    expect_equal(pred_fuzzy$"YSE.cubic", unname(pred_sharp$se.fit))
    
    # cutoff index
    cut_index <- newdat$X == cut
    
    # optimal
    pred_sharp <- predict(rd_sharp$model[[4]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.optimal"[cut_index], unname(pred_sharp$fit)[cut_index])
    expect_equal(pred_fuzzy$"YSE.optimal"[cut_index], unname(pred_sharp$se.fit)[cut_index])
    
    # half
    pred_sharp <- predict(rd_sharp$model[[5]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.half"[cut_index], unname(pred_sharp$fit)[cut_index])
    expect_equal(pred_fuzzy$"YSE.half"[cut_index], unname(pred_sharp$se.fit)[cut_index])
    
    # double
    pred_sharp <- predict(rd_sharp$model[[6]], newdata = newdat, se.fit = T)
    
    expect_equal(pred_fuzzy$"Yhat.double"[cut_index], unname(pred_sharp$fit)[cut_index])
    expect_equal(pred_fuzzy$"YSE.double"[cut_index], unname(pred_sharp$se.fit)[cut_index])
    
    # all
    # all.equal(summary(rd_sharp), summary(rd_fuzzy))
    
    # prediction difference 0/1 = estimation
    pred_treat <- subset(pred_fuzzy, X == cut & Tr == 1, 
      select = c(Yhat.linear, Yhat.quadratic, Yhat.cubic, Yhat.optimal, Yhat.half, Yhat.double))
    pred_control <- subset(pred_fuzzy, X == cut & Tr == 0, 
      select = c(Yhat.linear, Yhat.quadratic, Yhat.cubic, Yhat.optimal, Yhat.half, Yhat.double))
    
    expect_equal(unname(rd_fuzzy$est), unname(unlist(pred_treat - pred_control)))
    
  })

  # fuzzy rdd

  dat_fuzzy <- dat
  dat_fuzzy$tr <- as.integer(dat_fuzzy$tr | rbinom(nrow(dat_fuzzy), 1, .2))
  
  test_that("sharp LATE vs. fuzzy ITT", {
    rd_fuzzy <- rddapp::rd_est(y ~ x + tr | cov, data = dat_fuzzy, cutpoint = cut, 
        t.design = t.design) 
    rd_fuzzy_itt <- rddapp::rd_est(y ~ x + tr | cov, data = dat_fuzzy, cutpoint = cut, 
        t.design = t.design, est.itt = T) 
    rd_sharp <- rddapp::rd_est(y ~ x | cov, data = dat_fuzzy, cutpoint = cut, 
        t.design = t.design) 
    
    pred_fuzzy_itt <- predict(rd_fuzzy_itt)
    pred_sharp <- predict(rd_sharp)
    expect_equal(pred_fuzzy_itt, pred_sharp)
    
  })
  
}



