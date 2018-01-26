library(AER)
library(sandwich)
library(lmtest)
library(Formula)
library(rddapp)
library(rdd)
library(rddtools)

# data

set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
z <- rbinom(1000, 1, 0.5)

# rddapp vs. rdd

rddapp_model <- rddapp::RDestimate(y ~ x + z, cutpoint = 0)
rdd_model <- rdd::RDestimate(y ~ x + z, cutpoint = 0)

all.equal(unname(rddapp_model$est)[2:4], unname(rdd_model$est)[1:3]) # estimate
all.equal(unname(rddapp_model$bw)[2:4], unname(rdd_model$bw)[1:3]) # bandwidth
all.equal(unname(rddapp_model$se)[2:4], unname(rdd_model$se)[1:3]) # standard error
all.equal(unname(rddapp_model$obs)[2:4], unname(rdd_model$obs)[1:3]) # observation

rddapp_model <- rddapp::RDestimate(y ~ x + z, cutpoint = 0.1)
rdd_model <- rdd::RDestimate(y ~ x + z, cutpoint = 0.1)

all.equal(unname(rddapp_model$est)[2:4], unname(rdd_model$est)[1:3]) # estimate
all.equal(unname(rddapp_model$bw)[2:4], unname(rdd_model$bw)[1:3]) # bandwidth
all.equal(unname(rddapp_model$se)[2:4], unname(rdd_model$se)[1:3]) # standard error
all.equal(unname(rddapp_model$obs)[2:4], unname(rdd_model$obs)[1:3]) # observation

rddapp_model <- rddapp::RDestimate(y ~ x + z, bw = 0.8)
rdd_model <- rdd::RDestimate(y ~ x + z, bw = 0.8)

all.equal(unname(rddapp_model$est)[2:4], unname(rdd_model$est)[1:3]) # estimate
all.equal(unname(rddapp_model$bw)[2:4], unname(rdd_model$bw)[1:3]) # bandwidth
all.equal(unname(rddapp_model$se)[2:4], unname(rdd_model$se)[1:3]) # standard error
all.equal(unname(rddapp_model$obs)[2:4], unname(rdd_model$obs)[1:3]) # observation

# rddapp vs. rddtools

rddapp_model <- rddapp::RDestimate(y ~ x + z, cutpoint = 0, se.type = "const")
data_model <- rddtools::rdd_data(x = x, y = y, z = z, cutpoint = 0)
rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
rddtools_test <- lmtest::coeftest(rddtools_model, 
                                  vcov. = sandwich::vcovHC(rddtools_model, "const"))

unname(rddapp_model$est)[1] == rddtools_test[2, 1] # estimate 
unname(rddapp_model$se)[1] == rddtools_test[2, 2] # standard error

rddapp_model <- rddapp::RDestimate(y ~ x + z, cutpoint = 0, se.type = "HC1")
data_model <- rddtools::rdd_data(x = x, y = y, z = z, cutpoint = 0)
rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
rddtools_test <- lmtest::coeftest(rddtools_model, 
                                  vcov. = sandwich::vcovHC(rddtools_model, "HC1"))

unname(rddapp_model$est)[1] == rddtools_test[2, 1] # estimate 
unname(rddapp_model$se)[1] == rddtools_test[2, 2] # standard error

rddapp_model <- rddapp::RDestimate(y ~ x + z, cutpoint = 0.1, se.type = "HC0")
data_model <- rddtools::rdd_data(x = x, y = y, z = z, cutpoint = 0.1)
rddtools_model <- rddtools::rdd_reg_lm(data_model, slope = "separate")
rddtools_test <- lmtest::coeftest(rddtools_model, 
                                  vcov. = sandwich::vcovHC(rddtools_model, "HC0"))

unname(rddapp_model$est)[1] == rddtools_test[2, 1] # estimate 
unname(rddapp_model$se)[1] == rddtools_test[2, 2] # standard error

# rddtools

bw <- rdd::IKbandwidth(X = x, Y = y, cutpoint = 0, kernel = "triangular", verbose = F)
kw <- rdd::kernelwts(X = x, center = 0, bw = bw, kernel = "triangular")

# rddtools

library(rddtools)

data_rdd <- rdd_data(x = x, y = y, z = z, cutpoint = 0)
rdd_model <- rdd_reg_lm(data_rdd, slope = "separate", weights = kw)
summary(rdd_model)

Kernel_tri <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1 - (abs(X - center)/bw))
}

bw <- rdd_bw_ik(data_rdd, kernel = "Triangular")
kw <- Kernel_tri(X = x, center = 0, bw = bw)

library(sandwich)

coeftest(rdd_model, vcov. = vcovHC(rdd_model, type = "HC0"))
coeftest(rdd_model, vcov. = vcovHC(rdd_model, type = "HC1"))

# np

library(np)

data_rdd <- rdd_data(x = x, y = y, z = z, cutpoint = 0)
rdd_model <- rdd_reg_np(data_rdd, slope = "separate", inference = "lm", bw = bw)
summary(rdd_model)

data_rdd <- rdd_data(x = x, y = y, z = z, cutpoint = 0)
rdd_model <- rdd_reg_np(data_rdd, slope = "separate", inference = "np", bw = bw)
summary(rdd_model)

# RDestimate from rdd

formula = y ~ x + z
data = data_rdd
subset = NULL
cutpoint = NULL
bw = NULL
kernel = "triangular"
se.type = "HC1"
cluster = NULL
verbose = FALSE
model = FALSE
frame = FALSE

call <- match.call()
data <- environment(formula)
formula <- as.Formula(formula)
X <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[1]]
Y <- model.frame(formula, rhs = 0, lhs = NULL, data = data, na.action = na.pass)[[1]]
if (!is.null(subset)) {
  X <- X[subset]
  Y <- Y[subset]
  if (!is.null(cluster)) 
    cluster <- cluster[subset]
}
if (!is.null(cluster)) {
  cluster <- as.character(cluster)
  robust.se <- function(model, cluster) {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
    rcse.cov <- dfc * sandwich(model, meat. = crossprod(uj)/N)
    rcse.se <- coeftest(model, rcse.cov)
    return(rcse.se[2, 2])
  }
}
na.ok <- complete.cases(X) & complete.cases(Y)
if (length(all.vars(formula(formula, rhs = 1, lhs = F))) > 1) {
  type <- "fuzzy"
  Z <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[2]]
  if (!is.null(subset)) 
    Z <- Z[subset]
  na.ok <- na.ok & complete.cases(Z)
  if (length(all.vars(formula(formula, rhs = 1, lhs = F))) > 2) 
    stop("Invalid formula. Read ?RDestimate for proper syntax")
} else {
  type = "sharp"
}
covs <- NULL
if (length(formula)[2] > 1) {
  covs <- model.frame(formula, rhs = 2, lhs = 0, data = data, na.action = na.pass)
  if (!is.null(subset)) 
    covs <- subset(covs, subset)
  na.ok <- na.ok & complete.cases(covs)
  covs <- subset(covs, na.ok)
}
X <- X[na.ok]
Y <- Y[na.ok]
if (type == "fuzzy") Z <- as.double(Z[na.ok])

if (is.null(cutpoint)) {
  cutpoint <- 0
  if (verbose) 
    cat("No cutpoint provided. Using default cutpoint of zero.\n")
}
if (frame) {
  if (type == "sharp") {
    if (!is.null(covs)) 
      dat.out <- data.frame(X, Y, covs) else dat.out <- data.frame(X, Y)
  } else {
    if (!is.null(covs)) 
      dat.out <- data.frame(X, Y, Z, covs) else dat.out <- data.frame(X, Y, Z)
  }
}
if (is.null(bw)) {
  bw <- IKbandwidth(X = X, Y = Y, cutpoint = cutpoint, kernel = kernel, verbose = verbose)
  bws <- c(bw, 0.5 * bw, 2 * bw)
  names(bws) <- c("LATE", "Half-BW", "Double-BW")
} else if (length(bw) == 1) {
  bws <- c(bw, 0.5 * bw, 2 * bw)
  names(bws) <- c("LATE", "Half-BW", "Double-BW")
} else {
  bws <- bw
}

# Setup values to be returned
o <- list()
o$type <- type
o$call <- call
o$est <- vector(length = length(bws), mode = "numeric")
names(o$est) <- names(bws)
o$bw <- as.vector(bws)
o$se <- vector(mode = "numeric")
o$z <- vector(mode = "numeric")
o$p <- vector(mode = "numeric")
o$obs <- vector(mode = "numeric")
o$ci <- matrix(NA, nrow = length(bws), ncol = 2)
o$model <- list()
if (type == "fuzzy") {
  o$model$firststage <- list()
  o$model$iv <- list()
}
o$frame <- list()
o$na.action <- which(na.ok == FALSE)
class(o) <- "RD"
X <- X - cutpoint
Xl <- (X < 0) * X
Xr <- (X >= 0) * X
Tr <- as.integer(X >= 0)


ibw <- which(bw == bws)
# Subset to within the bandwidth, except for when using gaussian weighting
sub <- X >= (-bw) & X <= (+bw)


if (kernel == "gaussian") sub <- TRUE

w <- kernelwts(X, 0, bw, kernel = kernel)
o$obs[ibw] <- sum(w > 0)

if (type == "sharp") {
  if (verbose) {
    cat("Running Sharp RD\n")
    cat("Running variable:", all.vars(formula(formula, rhs = 1, lhs = F))[1], 
      "\n")
    cat("Outcome variable:", all.vars(formula(formula, rhs = F, lhs = 1))[1], 
      "\n")
    if (!is.null(covs)) 
      cat("Covariates:", paste(names(covs), collapse = ", "), "\n")
  }
  if (!is.null(covs)) {
    data <- data.frame(Y, Tr, Xl, Xr, covs, w)
    form <- as.formula(paste("Y~Tr+Xl+Xr+", paste("Tr*", names(covs), collapse = "+", 
      sep = ""), sep = ""))
  } else {
    data <- data.frame(Y, Tr, Xl, Xr, w)
    form <- as.formula(Y ~ Tr + Xl + Xr)
  }
  
  mod <- lm(form, weights = w, data = subset(data, w > 0))
  if (verbose == TRUE) {
    cat("Model:\n")
    print(summary(mod))
  }
  o$est[ibw] <- coef(mod)["Tr"]
  if (is.null(cluster)) {
    o$se[ibw] <- coeftest(mod, vcovHC(mod, type = se.type))[2, 2]
  } else {
    o$se[ibw] <- robust.se(mod, cluster[na.ok][w > 0])
  }
  o$z[ibw] <- o$est[ibw]/o$se[ibw]
  o$p[ibw] <- 2 * pnorm(abs(o$z[ibw]), lower.tail = F)
  o$ci[ibw, ] <- c(o$est[ibw] - qnorm(0.975) * o$se[ibw], o$est[ibw] + qnorm(0.975) * 
    o$se[ibw])
  
  if (model) 
    o$model[[ibw]] = mod
  if (frame) 
    o$frame[[ibw]] = dat.out
  
} else {
  if (verbose) {
    cat("Running Fuzzy RD\n")
    # CLEAN UP
    cat("Running variable:", all.vars(formula(formula, rhs = 1, lhs = F))[1], 
      "\n")
    cat("Outcome variable:", all.vars(formula(formula, rhs = F, lhs = 1))[1], 
      "\n")
    cat("Treatment variable:", all.vars(formula(formula, rhs = 1, lhs = F))[2], 
      "\n")
    if (!is.null(covs)) 
      cat("Covariates:", paste(names(covs), collapse = ", "), "\n")
  }
  
  if (!is.null(covs)) {
    data <- data.frame(Y, Tr, Xl, Xr, Z, covs, w)
    form <- as.Formula(paste("Y~Z+Xl+Xr+", paste(names(covs), collapse = "+"), 
      "|Tr+Xl+Xr+", paste(names(covs), collapse = "+"), sep = ""))
    form1 <- as.Formula(paste("Z~Tr+Xl+Xr+", paste("Tr*", names(covs), collapse = "+", 
      sep = "")))
  } else {
    data <- data.frame(Y, Tr, Xl, Xr, Z, w)
    form <- as.Formula(Y ~ Z + Xl + Xr | Tr + Xl + Xr)
    form1 <- as.formula(Z ~ Tr + Xl + Xr)
  }
  
  mod1 <- lm(form1, weights = w, data = subset(data, w > 0))
  mod <- ivreg(form, weights = w, data = subset(data, w > 0))
}

coeftest(mod)
coeftest(mod, vcov. = vcovHC(mod, type = "HC0"))
coeftest(mod, vcov. = vcovHC(mod, type = "HC1"))

# rdd_reg_lm from rddtools

source("C:/Academia/Cornell/QML/RDD/RDD/reference/rddtools_bquast/R/get_methods.R")

rdd_object = data_rdd
covariates = NULL
order = 1
bw = NULL
slope = "separate"
covar.opt = list(strategy = c("include", "residual"), slope = c("same", "separate"), 
  bw = NULL)
covar.strat = c("include", "residual")
weights = kw

checkIsRDD(rdd_object)
cutpoint <- getCutpoint(rdd_object)
type <- getType(rdd_object)

if (!missing(covar.strat)) warning("covar.strat is (soon) deprecated arg!")
if (!missing(weights) & !is.null(bw)) stop("Cannot give both 'bw' and 'weights'")

## Subsetting
dat <- as.data.frame(rdd_object)

if (!is.null(bw)) {
  weights <- ifelse(dat$x >= cutpoint - bw & dat$x <= cutpoint + bw, 1, 0)
} else if (!missing(weights)) {
  weights <- weights
} else {
  weights <- NULL
}

## Construct data
if (missing(weights)) weights <- NULL
dat_step1 <- model.matrix(rdd_object, covariates = covariates, order = order, bw = bw, 
  slope = slope, covar.opt = covar.opt)

## Regression
if (type == "Sharp") {
  reg <- lm(y ~ ., data = dat_step1, weights = weights)
  class_reg <- "lm"
} else {
  if (!is.null(covariates)) 
    stop("Covariates currently not implemented for Fuzzy case")
  reg <- ivreg(y ~ . - ins | . - D, data = dat_step1, weights = weights)
  class_reg <- "ivreg"
}

coeftest(reg)
coeftest(reg, vcov. = vcovHC(reg, type = "HC0"))
coeftest(reg, vcov. = vcovHC(reg, type = "HC1")) 
