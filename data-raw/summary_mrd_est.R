## Exploring the nonparametrics output of mrd_est()
## to display it in shiny
## Author: Irena Papst
## 2018-10-23

library(rddapp)
# read data
load("~/git-repos/rddapp/data-raw/sim_data.Rdata")

## add treatment col to data and export as csv
## (for testing shiny w/ uploaded data)
## dplyr's mutate not working ?? (maybe bug in update)
# trt <- ifelse(dat$x1 >= c1 & dat$x2 >= c2, 1, 0)
# dat <- cbind(dat, trt)
# save(list=c("dat", "c1", "c2"),
#      file="~/git-repos/rddapp/data-raw/sim_data.Rdata")
# write.csv(dat, file="~/Desktop/sim_data.csv")

# estimate rdd
m <- mrd_est(y ~ x1 + x2, data = dat, cutpoint = c(c1, c2), kernel = "triangular", 
             se.type = "HC1",  t.design = t.design, est.cov = TRUE)
summary(m)

