library(rddapp)
library(AER)
library(lmtest)
library(Formula)

set.seed(12345)

x1 <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x1 + 3 * cov + 10 * (x2 >= 0) + rnorm(1000)
group <- rep(1:10, each = 100)

formula = y ~ x1 + x2 | cov

subset = NULL; cutpoint = NULL; bw = NULL; 
kernel = "triangular"; se.type = "HC1"; cluster = NULL; 
impute = group; verbose = FALSE; less = FALSE; est.cov = FALSE;
est.itt = FALSE; local = 0.15; ngrid = 2500; margin = 0.03; 
boot = NULL; method = c("center", "univ", "front"); 
t.design = c('l', 'l')

m1 <- mrd_impute(y ~ x1 + x2 | cov, impute = group, method = "center")
m2 <- mrd_est(y ~ x1 + x2 | cov, method = "center")

m1 <- mrd_impute(y ~ x1 + x2 | cov, impute = group, method = "univ")
m2 <- mrd_est(y ~ x1 + x2 | cov, method = "univ")

m1 <- mrd_impute(y ~ x1 + x2 | cov, impute = group, method = "front", boot = 100, local = 0.3)
m2 <- mrd_est(y ~ x1 + x2 | cov, method = "front", boot = 100, local = 0.3)





