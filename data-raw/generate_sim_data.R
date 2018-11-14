## Generate multivar rdd data for testing shiny
## Author: Irena Papst
## 2018-10-23

library(rddapp)

## Define sim_data() function to generate simulated data
## adapted from Vivian's simdata.do
sim_data = function(
  n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
  b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2,
  b_tr1x1 = 0 , b_tr2x2 = 0, b_tr1x1x2 = 0, b_tr2x1x2 = 0, seed = 1000) {
  set.seed(seed)
  dat = MASS::mvrnorm(n, 
                      mu = c(mu1, mu2), 
                      Sigma = matrix(c(sigma1 ^ 2, rep(corr * sigma1 * sigma2, 2), sigma2 ^ 2), 2, 2))
  
  x1 = dat[,1]
  x2 = dat[,2]
  
  # treatment
  x1_tr <- rddapp:::treat_assign(x1, c1, t.design[1])
  x2_tr <- rddapp:::treat_assign(x2, c2, t.design[2])
  
  tr1 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & !x2_tr, 1, 0))
  tr2 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(!x1_tr & x2_tr, 1, 0))
  trb <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & x2_tr, 1, 0))
  
  tr = ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
  
  # center assignments
  cx1 = x1 - c1
  cx2 = x2 - c2
  
  # outcome
  y = 
    const     * 1  + 
    b_x1      * cx1 + 
    b_x2      * cx2 + 
    b_x1x2    * cx1 * cx2 + 
    b_tr      * tr - 
    b_tr1     * tr1 - 
    b_tr2     * tr2 - 
    b_tr1x1	  * tr1 * cx1 - 
    b_tr1x2   * tr1 * cx2 - 
    b_tr2x1   * tr2 * cx1 - 
    b_tr2x2   * tr2 * cx2 - 
    b_tr1x1x2 * tr1 * cx1 * cx2 - 
    b_tr2x1x2 * tr2 * cx1 * cx2 + 
    rnorm(n,0,1)
  
  trt <- ifelse(x1 >= c1 & x2 >= c2, 1, 0)
  
  return(data.frame(x1, x2, y, trt))
}

## Sample size
n <- 1000

## Model coefficients
const <- 70
b_x1 <- 2
b_x2 <- 1
b_x1x2 <- 0
b_tr <- 8
b_tr1 <- 2
b_tr2 <- -2
b_tr1x1	<- 0
b_tr1x2 <- 0
b_tr2x1 <- 0
b_tr2x2 <- 0
b_tr1x1x2 <- 0
b_tr2x1x2 <- 0

## Assignments
c1 <- 40
c2 <- 60
t.design <- c('l','l')

## Population parameters
mu1 <- 45
sigma1 <- 10
mu2 <- 55
sigma2 <- 10
corr <- 0

## Generate data
dat <- sim_data(n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
                b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2)

## Save as .Rdata and .csv
save(list=c("dat", "c1", "c2"),
     file="~/git-repos/rddapp/data-raw/sim_data.Rdata")
write.csv(dat, file="~/git-repos/rddapp/data-raw/sim_data.csv",
          row.names = FALSE)