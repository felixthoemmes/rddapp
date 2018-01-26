rd_power_for <- function(num.rep = 100, sample.size = 100, 
                         x.dist = 'normal', x.para = c(0, 1), x.cut = 0,
                         x.fuzzy = c(0, 0), coeff = c(0, 1, 0.2, 0.1), 
                         eta.sq = 0.5, alpha.list = c(0.001, 0.01, 0.05)) {
  res <- matrix(NA, num.rep, 4)
  
  for (i in 1:num.rep) {
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
      t_var <- ifelse(x_var >= cut, 1, 0)
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
    
    # generate outcome before noise
    y_out <- coeff[1] + coeff[2] * t_var + coeff[3] * x_var + coeff[4] * t_var * x_var
    
    if (x.dist == 'normal') {
      E_X <- x.para[1]
      Var_X <- x.para[2]^2
      E_T <- (1 - x.fuzzy[1]) * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = FALSE) + 
        x.fuzzy[2] * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = TRUE)
      Var_T <- E_T - E_T^2
      
      # int_order_1 <- function(v) {
      #   v * dnorm(v, mean = x.para[1], sd = x.para[2])
      # }
      # E_XT <- (1 - x.fuzzy[1]) * integrate(int_order_1, lower = x.cut, upper = Inf)$value + 
      #   x.fuzzy[2] * integrate(int_order_1, lower = -Inf, upper = x.cut)$value
      # 
      # int_order_2 <- function(v) {
      #   v^2 * dnorm(v, mean = x.para[1], sd = x.para[2])
      # }
      # E_XT2 <- (1 - x.fuzzy[1]) * integrate(int_order_2, lower = x.cut, upper = Inf)$value + 
      #   x.fuzzy[2] * integrate(int_order_2, lower = -Inf, upper = x.cut)$value
      # 
      # Var_XT <- E_XT2 - E_XT^2
      # Cov_T_X <- E_XT - E_T * E_X
      # Cov_T_XT <- E_XT - E_T * E_XT
      # Cov_X_XT <- E_XT2 - E_X * E_XT
      # Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT + 
      #   2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT + 
      #   2 * coeff[3] * coeff[4] * Cov_X_XT
      
      Var_noise <- Var_T * (1 / eta.sq - 1)
      
    } else if (x.dist == 'uniform') {
      E_X <- (x.para[1] + x.para[2]) / 2
      Var_X <- (x.para[1] - x.para[2])^2 / 12
      E_T <- (1 - x.fuzzy[1]) * (x.para[2] - x.cut) / (x.para[2] - x.para[1]) + 
        x.fuzzy[2] * (x.cut - x.para[1]) / (x.para[2] - x.para[1])
      Var_T <- E_T - E_T^2
      
      # E_XT <- (1 - x.fuzzy[1]) * (x.para[2]^2 - x.cut^2) / 2 / (x.para[2] - x.para[1]) + 
      #   x.fuzzy[2] * (x.cut^2 - x.para[1]^2) / 2 / (x.para[2] - x.para[1])
      # E_XT2 <- (1 - x.fuzzy[1]) * (x.para[2]^3 - x.cut^3) / 3 / (x.para[2] - x.para[1]) + 
      #   x.fuzzy[2] * (x.cut^3 - x.para[1]^3) / 3 / (x.para[2] - x.para[1])
      # Var_XT <- E_XT2 - E_XT^2
      # Cov_T_X <- E_XT - E_T * E_X
      # Cov_T_XT <- E_XT - E_T * E_XT
      # Cov_X_XT <- E_XT2 - E_X * E_XT
      # Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT + 
      #   2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT + 
      #   2 * coeff[3] * coeff[4] * Cov_X_XT
      
      Var_noise <- Var_T * (1 / eta.sq - 1)
      
    } else {
      stop("distribution of assignment variable is not valid")
    }
    
    # generate outcome after noise
    if (Var_noise >= 0) {
      y_out <- y_out + rnorm(sample.size, 0, sqrt(Var_noise))
    } else {
      stop("partial eta-squared is not in an appropriate range")
    }
    
    # lm_model <- lm(y_out ~ t_var+x_var+t_var:x_var)
    # print(summary(lm_model))
    
    rd_model = try(if(x.fuzzy[1] == 1 && x.fuzzy[2] == 1) {
      # sharp design
      rddapp::rd_est(y_out ~ x_var, cutpoint = x.cut, less = TRUE)
    } else {
      # fuzzy design
      rddapp::rd_est(y_out ~ x_var + t_var, cutpoint = x.cut, less = TRUE)
    })
    
    # if (class(rd_model) == 'try-error') {
    #   est_res[i, ] <- NA
    #   pval_res[i, ] <- NA
    # } else {
    #   est_res[i, ] <- rd_model$est
    #   pval_res[i, ] <- rd_model$p
    # }
    
    res[i, ] <- c(rd_model$est, rd_model$p)
  }
  
  # comb_res <- matrix(NA, 2, 3 + length(alpha.list))
  # comb_res[, 1] <- colSums(!is.na(est_res))
  # comb_res[, 2] <- colMeans(est_res, na.rm = T)
  # comb_res[, 3] <- apply(est_res, 2, var, na.rm = T)
  # 
  # for (i in 1:length(alpha.list)) {
  #   alpha <- alpha.list[i]
  #   comb_res[, 3 + i] <- colMeans(pval_res < alpha, na.rm = T) 
  # }
  # 
  # rownames(comb_res) <- c("Linear", "Opt")
  # colnames(comb_res) <- c("success", "mean(est)", "var(est)", as.character(alpha.list))
  
  return(res)  
}

rd_power_rep <- function(sample.size = 100, 
                            x.dist = 'normal', x.para = c(0, 1), x.cut = 0,
                            x.fuzzy = c(0, 0), coeff = c(0, 1, 0.2, 0.1), 
                            eta.sq = 0.5, alpha.list = c(0.001, 0.01, 0.05)) {

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
  
  # generate outcome before noise
  y_out <- coeff[1] + coeff[2] * t_var + coeff[3] * x_var + coeff[4] * t_var * x_var
  
  if (x.dist == 'normal') {
    E_X <- x.para[1]
    Var_X <- x.para[2]^2
    E_T <- (1 - x.fuzzy[1]) * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = FALSE) + 
      x.fuzzy[2] * pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = TRUE)
    Var_T <- E_T - E_T^2
    
    # int_order_1 <- function(v) {
    #   v * dnorm(v, mean = x.para[1], sd = x.para[2])
    # }
    # E_XT <- (1 - x.fuzzy[1]) * integrate(int_order_1, lower = x.cut, upper = Inf)$value + 
    #   x.fuzzy[2] * integrate(int_order_1, lower = -Inf, upper = x.cut)$value
    # 
    # int_order_2 <- function(v) {
    #   v^2 * dnorm(v, mean = x.para[1], sd = x.para[2])
    # }
    # E_XT2 <- (1 - x.fuzzy[1]) * integrate(int_order_2, lower = x.cut, upper = Inf)$value + 
    #   x.fuzzy[2] * integrate(int_order_2, lower = -Inf, upper = x.cut)$value
    # 
    # Var_XT <- E_XT2 - E_XT^2
    # Cov_T_X <- E_XT - E_T * E_X
    # Cov_T_XT <- E_XT - E_T * E_XT
    # Cov_X_XT <- E_XT2 - E_X * E_XT
    # Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT + 
    #   2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT + 
    #   2 * coeff[3] * coeff[4] * Cov_X_XT
    
    Var_noise <- Var_T * (1 / eta.sq - 1)
    
  } else if (x.dist == 'uniform') {
    E_X <- (x.para[1] + x.para[2]) / 2
    Var_X <- (x.para[1] - x.para[2])^2 / 12
    E_T <- (1 - x.fuzzy[1]) * (x.para[2] - x.cut) / (x.para[2] - x.para[1]) + 
      x.fuzzy[2] * (x.cut - x.para[1]) / (x.para[2] - x.para[1])
    Var_T <- E_T - E_T^2
    
    # E_XT <- (1 - x.fuzzy[1]) * (x.para[2]^2 - x.cut^2) / 2 / (x.para[2] - x.para[1]) + 
    #   x.fuzzy[2] * (x.cut^2 - x.para[1]^2) / 2 / (x.para[2] - x.para[1])
    # E_XT2 <- (1 - x.fuzzy[1]) * (x.para[2]^3 - x.cut^3) / 3 / (x.para[2] - x.para[1]) + 
    #   x.fuzzy[2] * (x.cut^3 - x.para[1]^3) / 3 / (x.para[2] - x.para[1])
    # Var_XT <- E_XT2 - E_XT^2
    # Cov_T_X <- E_XT - E_T * E_X
    # Cov_T_XT <- E_XT - E_T * E_XT
    # Cov_X_XT <- E_XT2 - E_X * E_XT
    # Var_Y <- coeff[2]^2 * Var_T + coeff[3]^2 * Var_X + coeff[4]^2 * Var_XT + 
    #   2 * coeff[2] * coeff[3] * Cov_T_X + 2 * coeff[2] * coeff[4] * Cov_T_XT + 
    #   2 * coeff[3] * coeff[4] * Cov_X_XT
    
    Var_noise <- Var_T * (1 / eta.sq - 1)
    
  } else {
    stop("distribution of assignment variable is not valid")
  }
  
  # generate outcome after noise
  if (Var_noise >= 0) {
    y_out <- y_out + rnorm(sample.size, 0, sqrt(Var_noise))
  } else {
    stop("partial eta-squared is not in an appropriate range")
  }
  
  # lm_model <- lm(y_out ~ t_var+x_var+t_var:x_var)
  # print(summary(lm_model))
  
  rd_model = try(if(x.fuzzy[1] == 1 && x.fuzzy[2] == 1) {
    # sharp design
    rddapp::rd_est(y_out ~ x_var, cutpoint = x.cut, less = TRUE)
  } else {
    # fuzzy design
    rddapp::rd_est(y_out ~ x_var + t_var, cutpoint = x.cut, less = TRUE)
  })
  
  # if (class(rd_model) == 'try-error') {
  #   est_res[i, ] <- NA
  #   pval_res[i, ] <- NA
  # } else {
  #   est_res[i, ] <- rd_model$est
  #   pval_res[i, ] <- rd_model$p
  # }
  
  return(c(rd_model$est, rd_model$p))  
}

set.seed(1)
t1 <- proc.time()
res1 <- rd_power_for(num.rep = 1000)
proc.time() - t1 # 107.15

set.seed(1)
t2 <- proc.time()
res2 <- replicate(1000, rd_power_rep())
proc.time() - t2 # 103.79

res1[1, ] == res2[, 1]