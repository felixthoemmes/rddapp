#' Power Analysis of Regression Discontinuity
#' 
#' \code{rd_power} computes the empirical probability that RD is significant,
#' i.e. the empirical alpha of null hypothesis: RD = 0
#'
#' @param num.rep Number of repetitions used to calculate the empirical alpha.
#' @param sample.size Number of observations in each sample.
#' @param x.dist Distribution of the assignment variable X.
#'   \code{"normal"} distribution is the default. 
#'   \code{"uniform"} distribution is the only other option.
#' @param x.para Parameters of the distribution of the assignment variable X.
#'   If \code{x.dist} is \code{"normal"}, then \code{x.para} includes the
#'   mean and sd of normal distribution.
#'   If \code{x.dist} is \code{"uniform"}, then \code{x.para} includes the 
#'   upper and lower boundaries of uniform distribution.
#' @param x.cut Cutpoint of RD with respect to the assignment variable X.
#' @param x.fuzzy Probabilities to be assigned to control for individuals in treatment 
#'   based on cutoff, and to treatment for individuals in control based on cutoff.
#'   For a sharp design, by default, the 1st entry is 0, and the 2nd entry is 0. 
#'   For a fuzzy design, the 1st entry is the probability to be assigned to 
#'   control for individuals above the cutpoint, and the 2nd entry is the 
#'   probability to be assigned to treatment for individuals below the cutpoint.
#' @param x.design The treatment option according to design.
#'   The entry is for X: \code{"g"} means treatment is assigned 
#'   if X is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if X is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if X is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if X is less than or equal to its cutoff.
#' @param coeff Coefficients of variables in the linear model to generate data
#'   The 1st entry is the intercept. 
#'   The 2nd entry is the slope of treatment, i.e. treatment effect.
#'   The 3rd entry is the slope of assignment.
#'   The 4th entry is the slope of interaction between treatment and assignment.
#' @param eta.sq Expected partial eta-squared of the linear model with respect to the 
#'   treatment itself. It is used to control the variance of noise in the linear model.
#' @param alpha.list List of significance levels used to calculate the empirical alpha.
#'
#' @return \code{rd_power} returns the results of 2 estimators as a table, 
#'   including mean, variance, and power of estimate.
#'   The 1st \code{Linear} results of the linear regression estimator 
#'   The 2nd \code{Opt} results of the local linear regression estimator
#'   of RD, with the optimal bandwidth in the IK 2012 paper.  
#'
#' @include rd_est.R
#' @include treat_assign.R
#'
#' @importFrom stats rnorm runif rbinom
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rd_power()
#' rd_power(x.dist = "uniform", x.cut = 0.5)
#' rd_power(x.fuzzy = c(0.1, 0.1))
#' }

rd_power <- function(num.rep = 100, sample.size = 100, x.dist = "normal", x.para = c(0, 1), 
  x.cut = 0, x.fuzzy = c(0, 0), x.design = "l", coeff = c(0.3, 1, 0.2, 0.3), eta.sq = 0.5, 
  alpha.list = c(0.001, 0.01, 0.05)) {
  est_res <- matrix(NA, num.rep, 2)
  pval_res <- matrix(NA, num.rep, 2)
  
  if (!x.design %in% c("g", "geq", "l", "leq")) {
    stop("Treatment design must be one of 'g', 'geq', 'l', 'leq'.")
  }

  for (i in 1:num.rep) {
    # generate assignment variable
    if (x.dist == "normal") {
      x_var <- rnorm(sample.size, x.para[1], x.para[2])
    } else if (x.dist == "uniform") {
      x_var <- runif(sample.size, x.para[1], x.para[2])
    } else {
      stop("Distribution of assignment variable is not valid.")
    }
    
    # generate treatment variable
    # if (x.cut > min(x_var) && x.cut < max(x_var)) {
    if (x.fuzzy[1] == 0 && x.fuzzy[2] == 0) {
      # sharp design
      t_var <- treat_assign(x_var, x.cut, x.design)

    } else {
      # fuzzy design
      t_var <- rep(NA, sample.size)

      treat <- as.logical(treat_assign(x_var, x.cut, x.design))
      control <- !treat

      if (x.fuzzy[1] <= 1 && x.fuzzy[1] >= 0 && x.fuzzy[2] <= 1 
        && x.fuzzy[2] >= 0) {
        t_var[treat] <- rbinom(sum(treat), 1, 1 - x.fuzzy[1])
        t_var[control] <- rbinom(sum(control), 1, x.fuzzy[2])
      } else {
        stop("Fuzzy probability must between (>=) 0 and (<=) 1.")
      }
    }
    # } else {
    #   stop("cutpoint is not in the range of assignment variable")
    # }
    
    if (eta.sq > 0.99 || eta.sq < 0.01) {
      stop("eta.sq must between (>=) 0.01 and (<=) 0.99.")
    }
    
    # generate outcome before noise
    y_out <- coeff[1] + coeff[2] * t_var + coeff[3] * x_var + coeff[4] * t_var * x_var
    
    if (x.dist == "normal") {
      E_X <- x.para[1]
      Var_X <- x.para[2]^2
      E_T <- (1 - x.fuzzy[1]) * 
        pnorm(x.cut, mean = x.para[1], sd = x.para[2], lower.tail = FALSE) + 
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

    } else if (x.dist == "uniform") {
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
      stop("Distribution of assignment variable is not valid.")
    }

    # generate outcome after noise
    if (Var_noise >= 0) {
      y_out <- y_out + rnorm(sample.size, 0, sqrt(Var_noise))
    } else {
      stop("Partial eta-squared is not in an appropriate range.")
    }

    # lm_model <- lm(y_out ~ t_var+x_var+t_var:x_var)
    # print(summary(lm_model))
    
    rd_model <- try(if(x.fuzzy[1] == 0 && x.fuzzy[2] == 0) {
      # sharp design
      rd_est(y_out ~ x_var, cutpoint = x.cut, less = TRUE)
    } else {
      # fuzzy design
      rd_est(y_out ~ x_var + t_var, cutpoint = x.cut, less = TRUE)
    })
    
    if (class(rd_model) == "try-error") {
      est_res[i, ] <- NA
      pval_res[i, ] <- NA
    } else {
      est_res[i, ] <- rd_model$est
      pval_res[i, ] <- rd_model$p
    }

  }
  
  comb_res <- matrix(NA, 2, 3 + length(alpha.list))
  comb_res[, 1] <- colSums(!is.na(est_res))
  comb_res[, 2] <- colMeans(est_res, na.rm = TRUE)
  comb_res[, 3] <- apply(est_res, 2, var, na.rm = TRUE)

  for (i in 1:length(alpha.list)) {
    alpha <- alpha.list[i]
    comb_res[, 3 + i] <- colMeans(pval_res < alpha, na.rm = TRUE) 
  }
  
  rownames(comb_res) <- c("Linear", "Opt")
  colnames(comb_res) <- c("success", "mean(est)", "var(est)", as.character(alpha.list))
  
  return(comb_res)  
}







