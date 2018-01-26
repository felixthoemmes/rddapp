context("RDD Power Analysis Moment")

# library
library(AER)
library(sandwich)
library(lmtest)
library(Formula)
library(rddapp)
# library(rdd)
# library(rddtools)

# based on rd_power in rddapp
rd_power_moment <- function(sample.size = 100, x.dist = 'normal', x.para = c(0, 1), x.cut = 0,
                            x.fuzzy = c(0, 0), coeff = c(0, 1, 0.2, 0.1)) {
  
  # generate assignment variable
  if (x.dist == 'normal') {
    x_var <- rnorm(sample.size, x.para[1], x.para[2])
  } else if (x.dist == 'uniform') {
    x_var <- runif(sample.size, x.para[1], x.para[2])
  } else {
    stop("distribution of assignment variable is not valid")
  }
  
  # generate treatment variable
  # if (x.cut > min(x_var) && x.cut < max(x_var)) {
  if (x.fuzzy[1] == 1 && x.fuzzy[2] == 1) {
    # sharp design
    t_var <- ifelse(x_var >= x.cut, 1, 0)
  } else {
    # fuzzy design
    t_var <- rep(NA, sample.size)
    above <- x_var >= x.cut
    below <- x_var < x.cut
    if (x.fuzzy[1] <= 1 && x.fuzzy[1] >= 0 && x.fuzzy[2] <= 1 
        && x.fuzzy[2] >= 0) {
      t_var[above] <- rbinom(sum(above), 1, 1 - x.fuzzy[1])
      t_var[below] <- rbinom(sum(below), 1, x.fuzzy[2])
    } else {
      stop("fuzzy probability must between (>=) 0 and (<=) 1")
    }
  }
  # } else {
  #   stop("cutpoint is not in the range of assignment variable")
  # }
  
  # if (eta.sq > 0.99 || eta.sq < 0.01) {
  #   stop("eta.sq must between (>=) 0.01 and (<=) 0.99")
  # }
  
  # generate outcome before noise
  y_out <- coeff[1] + coeff[2] * t_var + coeff[3] * x_var + coeff[4] * t_var * x_var

  if (x.dist == 'normal') {
    E_X <- x.para[1]
    Var_X <- x.para[2]^2
    E_T <- (1 - x.fuzzy[1]) * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = FALSE) + 
      x.fuzzy[2] * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = TRUE)
    Var_T <- E_T - E_T^2
    
    int_order_1 <- function(v) {
      v * dnorm(v, mean = x.para[1], sd = x.para[2])
    }
    E_XT <- (1 - x.fuzzy[1]) * integrate(int_order_1, lower = x.cut, upper = Inf)$value +
      x.fuzzy[2] * integrate(int_order_1, lower = -Inf, upper = x.cut)$value

    int_order_2 <- function(v) {
      v^2 * dnorm(v, mean = x.para[1], sd = x.para[2])
    }
    E_XT2 <- (1 - x.fuzzy[1]) * integrate(int_order_2, lower = x.cut, upper = Inf)$value +
      x.fuzzy[2] * integrate(int_order_2, lower = -Inf, upper = x.cut)$value

    Var_XT <- E_XT2 - E_XT^2
    Cov_T_X <- E_XT - E_T * E_X
    Cov_T_XT <- E_XT - E_T * E_XT
    Cov_X_XT <- E_XT2 - E_X * E_XT
    Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT +
      2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT +
      2 * coeff[3] * coeff[4] * Cov_X_XT
    
    # Var_noise <- Var_T * (1 / eta.sq - 1)
    
  } else if (x.dist == 'uniform') {
    E_X <- (x.para[1] + x.para[2]) / 2
    Var_X <- (x.para[1] - x.para[2])^2 / 12
    E_T <- (1 - x.fuzzy[1]) * (x.para[2] - x.cut) / (x.para[2] - x.para[1]) + 
      x.fuzzy[2] * (x.cut - x.para[1]) / (x.para[2] - x.para[1])
    Var_T <- E_T - E_T^2
    
    E_XT <- (1 - x.fuzzy[1]) * (x.para[2]^2 - x.cut^2) / 2 / (x.para[2] - x.para[1]) +
      x.fuzzy[2] * (x.cut^2 - x.para[1]^2) / 2 / (x.para[2] - x.para[1])
    E_XT2 <- (1 - x.fuzzy[1]) * (x.para[2]^3 - x.cut^3) / 3 / (x.para[2] - x.para[1]) +
      x.fuzzy[2] * (x.cut^3 - x.para[1]^3) / 3 / (x.para[2] - x.para[1])
    Var_XT <- E_XT2 - E_XT^2
    Cov_T_X <- E_XT - E_T * E_X
    Cov_T_XT <- E_XT - E_T * E_XT
    Cov_X_XT <- E_XT2 - E_X * E_XT
    Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT +
      2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT +
      2 * coeff[3] * coeff[4] * Cov_X_XT
    
    # Var_noise <- Var_T * (1 / eta.sq - 1)
    
  } else {
    stop("distribution of assignment variable is not valid")
  }
  
  return(list(
    p = c(E_X, Var_X, E_T, Var_T, E_XT, 
          E_XT2, Var_XT, Cov_T_X, 
          Cov_T_XT, Cov_X_XT, Var_Y),
    s = c(mean(x_var), var(x_var), mean(t_var), var(t_var), mean(x_var * t_var),
          mean(x_var^2 * t_var), var(x_var * t_var), cov(t_var, x_var), 
          cov(t_var, x_var * t_var), cov(x_var, x_var * t_var), var(y_out))
    ))
}

#
set.seed(12345)

# sharp rdd

# normal (0, 1)
res <- rd_power_moment(sample.size = 10000, x.dist = 'normal', x.cut = 0, 
                         x.fuzzy = c(0, 0))
all.equal(res$p, res$s, tolerance = 5e-3)  

# uniform (0, 1)
res <- rd_power_moment(sample.size = 10000, x.dist = 'uniform', x.cut = 0.5, 
                         x.fuzzy = c(0, 0))
all.equal(res$p, res$s, tolerance = 5e-3)  

# fuzzy rdd

# normal (0, 1)
res <- rd_power_moment(sample.size = 10000, x.dist = 'normal', x.cut = 0, 
                         x.fuzzy = c(0.1, 0.1))
all.equal(res$p, res$s, tolerance = 8e-3)  

# uniform (0, 1)
rd_power_moment(sample.size = 10000, x.dist = 'uniform', x.cut = 0.5, 
                         x.fuzzy = c(0.1, 0.1))
all.equal(res$p, res$s, tolerance = 7e-3) 