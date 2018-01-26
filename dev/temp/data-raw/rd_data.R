#
library(mice)
setwd('C:/Academia/Cornell/QML/RDD/RDD/rdd/rddapp/data-raw')

###############################################
# sharp RDD with 1 assignment and no covariates
###############################################

set.seed(1234)  # set seed for reproducible results
n <- 1000  # sample size

# generate treatment assignment variable
x <- round(rnorm(n, 5, 2), 2)

# generate actual treatment based on cutoff x>=6
tr <- ifelse(x >= 6, 1, 0)

# centered x
xc <- x - 6

# generate outcome as function of x, tr, and random noise
y <- .3 * xc + 1 * tr + .2 * xc * tr + round(rnorm(n, 0, 1), 2)

s1nocov <- data.frame(x = x, tr = tr, y = y)
write.csv(s1nocov, "s1nocov.csv", quote = FALSE, row.names = FALSE)

#########################################################################################
# sharp RDD with 1 assignment and no covariates with missing data and multiple imputation
#########################################################################################

set.seed(1234) 
n <- 1000 

# generate treatment assignment variable
x <- round(rnorm(n, 5, 2), 2)

# generate actual treatment based on cutoff x>=6
tr <- ifelse(x >= 6, 1, 0)

# centered x
xc <- x - 6

# generate outcome as function of x, tr, and random noise
y <- .3 * xc + 1 * tr + .2 * xc * tr + round(rnorm(n, 0, 1), 2)

# induce MCAR missingness on both x and y
x <- ifelse(runif(1000, 0, 1) < .2, NA, x)
y <- ifelse(runif(1000, 0, 1) < .2, NA, y)

# now impute using mice
s1nocovmi <- data.frame(x = x, tr = tr, y = y)
m1 <- mice(s1nocovmi, method = "norm")
s1nocovmi <- complete(m1, action = "long")

write.csv(s1nocovmi, "s1nocovmi.csv", quote = FALSE, row.names = FALSE)

###############################################
# fuzzy RDD with 1 assignment and no covariates
###############################################

set.seed(1234) 
n <- 1000  

# generate treatment assignment variable
x <- round(rnorm(n, 5, 2), 2)

# generate actual treatment based on cutoff x>=6
tr <- rbinom(1000, 1, 1 / (1 + exp(-(2 + 3.5 * (x - 6)))))
# plot(x, 1 / (1 + exp(-(2 + 3.5 * (x-6)))))
# abline(v = 6)

# centered x
xc <- x - 6

# generate outcome as function of x, tr, and random noise
y <- .3 * xc + 1 * tr + .2 * xc * tr + round(rnorm(n, 0, 1), 2)
# plot(x, y, col = tr + 1)
# table(tr, x - 6 >= 0)

f1nocov <- data.frame(x = x, tr = tr, y = y)
write.csv(f1nocov, "f1nocov.csv", quote = FALSE, row.names = FALSE)

################################################
# sharp RDD with 2 assignments and no covariates
################################################

set.seed(1234)  
n <- 1000  

# generate treatment assignment variable 1
x1 <- round(rnorm(n, 5, 2), 2)
x2 <- round(rnorm(n, 100, 15), 2)

# generate actual treatment based on cutoff x1>=6 OR x2<=75
tr1 <- ifelse(x1 >= 6, 1, 0)
tr2 <- ifelse(x2 <= 75, 1, 0)

tr <- ifelse(x1 >= 6 | x2 <= 75, 1, 0)

# centered x1 and x2
x1c <- x1 - 6
x2c <- x2 - 75

# generate outcome as function of x, tr, and random noise
y <- .3 * x1c + .2 * x2c + .4 * tr1 + .5 * tr2 + .1 * x1c * tr1 + .1 * x2c * tr2 + 
  .1 * x1c * tr2 + .1 * x2c * tr1 + .6 * tr + round(rnorm(n, 0, .2), 2)

s2nocov <- data.frame(x1 = x1, x2 = x2, tr1 = tr1, tr2 = tr2, tr = tr, y = y)
write.csv(s2nocov, "s2nocov.csv", quote = FALSE, row.names = FALSE)

################################################
# fuzzy RDD with 2 assignments and no covariates
################################################

set.seed(1234) 
n <- 1000 

# generate treatment assignment variable 1
x1 <- round(rnorm(n, 5, 2), 2)
x2 <- round(rnorm(n, 100, 15), 2)

# generate actual treatment based on cutoff x1>=6 OR x2<=75
tr1 <- ifelse(x1 >= 6, 1, 0)
tr2 <- ifelse(x2 <= 75, 1, 0)

tr <- ifelse(x1 >= 6 | x2 <= 75, 1, 0)

# add fuzzyness manually
tr1[1:20] <- 1 - tr1[1:20]
tr2[20:30] <- 1 - tr2[20:30]
tr[30:40] <- 1 - tr[30:40]

# centered x1 and x2
x1c <- x1 - 6
x2c <- x2 - 75

# generate outcome as function of x, tr, and random noise
y <- .3 * x1c + .2 * x2c + .4 * tr1 + .5 * tr2 + .1 * x1c * tr1 + .1 * x2c * tr2 + 
  .1 * x1c * tr2 + .1 * x2c * tr1 + .6 * tr + round(rnorm(n, 0, .2), 2)

f2nocov <- data.frame(x1 = x1, x2 = x2, tr1 = tr1, tr2 = tr2, tr = tr, y = y)
write.csv(f2nocov, "f2nocov.csv", quote = FALSE, row.names = FALSE)

#############################################
# sharp RDD with 1 assignment and 1 covariate
#############################################

set.seed(1234)
n <- 1000 

# generate treatment assignment variable
x <- round(rnorm(n, 5, 2), 2)

# generate cov (unrelated to x)
cov <- round(rnorm(n, 0, 1), 2)

# generate actual treatment based on cutoff x>=6
tr <- ifelse(x >= 6, 1, 0)

# centered x
xc <- x - 6

# generate outcome as function of x, tr, and random noise
y <- .3 * xc + 1 * tr + .2 * xc * tr + .2 * cov + round(rnorm(n, 0, 1), 2)

s1cov <- data.frame(x = x, cov = cov, tr = tr, y = y)
write.csv(s1cov, "s1cov.csv", quote = FALSE, row.names = FALSE)

#############################################
# fuzzy RDD with 1 assignment and 1 covariate
#############################################

set.seed(1234)
n <- 1000

# generate treatment assignment variable
x <- round(rnorm(n, 5, 2), 2)

# generate cov (unrelated to x)
cov <- round(rnorm(n, 0, 1), 2)

# generate actual treatment based on cutoff x>=6
tr <- rbinom(1000, 1, 1 / (1 + exp(-(2 + 3.5 * (x-6)))))
# plot(x, 1 / (1 + exp(-(2 + 3.5 * (x-6)))))
# abline(v = 6)

# centered x
xc <- x - 6

# generate outcome as function of x, tr, and random noise
y <- .3 * xc + 1 * tr + .2 * xc * tr + .2 * cov + round(rnorm(n, 0, 1), 2)
# plot(x, y, col = tr + 1)
# table(tr, x - 6 >= 0)

f1cov <- data.frame(x = x, cov = cov, tr = tr, y = y)
write.csv(f1cov, "f1cov.csv", quote = FALSE, row.names = FALSE)

##############################################
# sharp RDD with 2 assignments and 1 covariate
##############################################

set.seed(1234)
n <- 1000 

# generate treatment assignment variable 1 and 2 (unrelated to each other)
x1 <- round(rnorm(n, 5, 2), 2)
x2 <- round(rnorm(n, 100, 15), 2)

# generate cov (unrelated to x1,x2)
cov <- round(rnorm(n, 0, 1), 2)

# generate actual treatment based on cutoff x1>=6 OR x2<=75
tr1 <- ifelse(x1 >= 6, 1, 0)
tr2 <- ifelse(x2 <= 75, 1, 0)

tr <- ifelse(x1 >= 6 | x2 <= 75, 1, 0)

# centered x1 nad x2
x1c <- x1 - 6
x2c <- x2 - 75

# generate outcome as function of x, tr, and random noise
y <- .3 * x1c + .2 * x2c + .4 * tr1 + .5 * tr2 + .1 * x1c * tr1 + .1 * x2c * tr2 + 
  .1 * x1c * tr2 + .1 * x2c * tr1 + .6 * tr + .2 * cov + round(rnorm(n, 0, .2), 2)

s2cov <- data.frame(x1 = x1, x2 = x2, cov = cov, tr1 = tr1, tr2 = tr2, tr = tr, y = y)
write.csv(s2cov, "s2cov.csv", quote = FALSE, row.names = FALSE)

##############################################
# fuzzy RDD with 2 assignments and 1 covariate
##############################################

set.seed(1234)
n <- 1000 

# generate treatment assignment variable 1 and 2 (unrelated to each other)
x1 <- round(rnorm(n, 5, 2), 2)
x2 <- round(rnorm(n, 100, 15), 2)

# generate cov (unrelated to x)
cov <- round(rnorm(n, 0, 1), 2)

# generate actual treatment based on cutoff x1>=6 OR x2<=75
tr1 <- ifelse(x1 >= 6, 1, 0)
tr2 <- ifelse(x2 <= 75, 1, 0)

tr <- ifelse(x1 >= 6 | x2 <= 75, 1, 0)

# add fuzzyness manually
tr1[1:20] <- 1 - tr1[1:20]
tr2[20:30] <- 1 - tr2[20:30]
tr[30:40] <- 1 - tr[30:40]

# centered x1 and x2
x1c <- x1 - 6
x2c <- x2 - 75

# generate outcome as function of x, tr, and random noise
y <- .3 * x1c + .2 * x2c + .4 * tr1 + .5 * tr2 + .1 * x1c * tr1 + .1 * x2c * tr2 + 
  .1 * x1c * tr2 + .1 * x2c * tr1 + .6 * tr + .2 * cov + round(rnorm(n, 0, .2), 2)

f2cov <- data.frame(x1 = x1, x2 = x2, cov = cov, tr = tr, y = y)
write.csv(f2cov, "f2cov.csv", quote = FALSE, row.names = FALSE)
