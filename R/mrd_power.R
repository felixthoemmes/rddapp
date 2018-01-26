#' Power Analysis of Multivariate Regression Discontinuity
#' 
#' \code{mrd_power} computes the empirical probability that RD is significant,
#' i.e. the empirical alpha of null hypothesis: RD = 0
#'
#' @param num.rep Number of repetitions used to calculate the empirical alpha.
#' @param sample.size Number of observations in each sample.
#' @param x1.dist Distribution of the 1st assignment variable X1.
#'   \code{"normal"} distribution is the default. 
#'   \code{"uniform"} distribution is the only other option.
#' @param x1.para Parameters of the distribution of the 1st assignment variable X1.
#'   If \code{x1.dist} is \code{"normal"}, then \code{x1.para} includes the
#'   mean and sd of normal distribution.
#'   If \code{x1.dist} is \code{"uniform"}, then \code{x1.para} includes the 
#'   upper and lower boundaries of uniform distribution.
#' @param x2.dist Distribution of the 2nd assignment variable X2.
#' @param x2.para Parameters of the distribution of the 2nd assignment variable X2.
#' @param x1.cut Cutpoint of RD with respect to the 1st assignment variable X1.
#' @param x2.cut Cutpoint of RD with respect to the 2nd assignment variable X2.
#' @param x1.fuzzy Probabilities to be assigned to control in terms of the 1st
#'   assignment variable X1 for individuals in treatment based on cutoff, 
#'   and to treatment for individuals in control based on cutoff.
#'   For a sharp design, by default, the 1st entry is 0, and the 2nd entry is 0. 
#'   For a fuzzy design, the 1st entry is the probability to be assigned to 
#'   control for individuals above the cutpoint, and the 2nd entry is the 
#'   probability to be assigned to treatment for individuals below the cutpoint.
#' @param x2.fuzzy Probabilities to be assigned to control in terms of the 2nd
#'   assignment variable X2 for individuals in treatment based on cutoff, 
#'   and to treatment for individuals in control based on cutoff.
#' @param x1.design The treatment option according to design.
#'   The entry is for X1: \code{"g"} means treatment is assigned 
#'   if X1 is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if X1 is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if X1 is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if X1 is less than or equal to its cutoff.
#' @param x2.design The treatment option according to design.
#'   The entry is for X2.
#' @param coeff Coefficients of variables in the linear model to generate data
#'   The 1st entry is the intercept. 
#'   The 2nd entry is the slope of treatment 1, i.e. treatment effect 1.
#'   The 3rd entry is the slope of treatment 2, i.e. treatment effect 2.
#'   The 4th entry is the slope of treatment, i.e. treatment effect.
#'   The 5th entry is the slope of assignment 1.
#'   The 6th entry is the slope of assignment 2.
#'   The 7th entry is the slope of interaction between assignment 1 and assignment 2.
#'   The 8th entry is the slope of interaction between treatment 1 and assignment 1.
#'   The 9th entry is the slope of interaction between treatment 2 and assignment 1.
#'   The 10th entry is the slope of interaction between treatment 1 and assignment 2.
#'   The 11th entry is the slope of interaction between treatment 2 and assignment 2.
#'   The 12th entry is the slope of interaction between treatment 1, assignment 1 and assignment 2.
#'   The 13th entry is the slope of interaction between treatment 2, assignment 1 and assignment 2.
#' @param eta.sq Expected partial eta-squared of the linear model with respect to the 
#'   treatment itself. It is used to control the variance of noise in the linear model.
#' @param alpha.list List of significance levels used to calculate the empirical alpha.
#'
#' @return \code{mrd_power} returns the results of 6 estimators as a table, 
#'   including mean, variance, and power of estimate.
#'   The 1st \code{Linear} results of the linear regression estimator 
#'   of combined RD using the centering approach.
#'   The 2nd \code{Opt} results of the local linear regression estimator
#'   of combined RD using the centering approach, 
#'   with the optimal bandwidth in the IK 2012 paper.
#'   The 3rd \code{Linear} results of the linear regression estimator 
#'   of separate RD in terms of x1 using the univariate approach.
#'   The 4th \code{Opt} results of the local linear regression estimator
#'   of separate RD in terms of x1 using the univariate approach, 
#'   with the optimal bandwidth in the IK 2012 paper.
#'   The 5th \code{Linear} results of the linear regression estimator 
#'   of separate RD in terms of x2 using the univariate approach.
#'   The 6th \code{Opt} results of the local linear regression estimator
#'   of separate RD in terms of x2 using the univariate approach, 
#'   with the optimal bandwidth in the IK 2012 paper. 
#'
#' @include mrd_est.R
#' @include treat_assign.R
#'
#' @importFrom stats rnorm runif rbinom
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mrd_power()
#' mrd_power(x1.dist = "uniform", x1.cut = 0.5)
#' mrd_power(x1.fuzzy = c(0.1, 0.1))
#' }

mrd_power <- function(num.rep = 100, sample.size = 100, x1.dist = "normal", x1.para = c(0, 1), 
  x2.dist = "normal", x2.para = c(0, 1), x1.cut = 0, x2.cut = 0, x1.fuzzy = c(0, 0), 
  x2.fuzzy = c(0, 0), x1.design = "l", x2.design = "l", coeff = c(0.1, 0.5, 0.5, 1, rep(0.1, 9)), 
  eta.sq = 0.5, alpha.list = c(0.001, 0.01, 0.05)) {
  est_res <- matrix(NA, num.rep, 6)
  pval_res <- matrix(NA, num.rep, 6)
  
  if (!all(c(x1.design, x2.design) %in% c("g", "geq", "l", "leq"))) {
    stop("Treatment design must be one of 'g', 'geq', 'l', 'leq'.")
  }

  for (i in 1:num.rep) {
    # generate assignment variable
    if (x1.dist == "normal") {
      x1_var <- rnorm(sample.size, x1.para[1], x1.para[2])
    } else if (x1.dist == "uniform") {
      x1_var <- runif(sample.size, x1.para[1], x1.para[2])
    } else {
      stop("Distribution of assignment variable is not valid.")
    }
    
    if (x2.dist == "normal") {
      x2_var <- rnorm(sample.size, x2.para[1], x2.para[2])
    } else if (x2.dist == "uniform") {
      x2_var <- runif(sample.size, x2.para[1], x2.para[2])
    } else {
      stop("Distribution of assignment variable is not valid.")
    }

    # generate treatment variable
    # if (cut > min(x_var) && cut < max(x_var)) {
    if (x1.fuzzy[1] == 0 && x1.fuzzy[2] == 0) {
      # sharp design
      t1_var <- treat_assign(x1_var, x1.cut, x1.design)

    } else {
      # fuzzy design
      t1_var <- rep(NA, sample.size)

      treat1 <- as.logical(treat_assign(x1_var, x1.cut, x1.design))
      control1 <- !treat1

      if (x1.fuzzy[1] <= 1 && x1.fuzzy[1] >= 0 && x1.fuzzy[2] <= 1 && x1.fuzzy[2] >= 0) {
        t1_var[treat1] <- rbinom(sum(treat1), 1, 1 - x1.fuzzy[1])
        t1_var[control1] <- rbinom(sum(control1), 1, x1.fuzzy[2])
      } else {
        stop("Fuzzy probability must between (>=) 0 and (<=) 1.")
      }
    }

    if (x2.fuzzy[1] == 0 && x2.fuzzy[2] == 0) {
      # sharp design
      t2_var <- treat_assign(x2_var, x2.cut, x2.design)

    } else {
      # fuzzy design
      t2_var <- rep(NA, sample.size)

      treat2 <- as.logical(treat_assign(x2_var, x2.cut, x2.design))
      control2 <- !treat2

      if (x2.fuzzy[1] <= 1 && x2.fuzzy[1] >= 0 && x2.fuzzy[2] <= 1 && x2.fuzzy[2] >= 0) {
        t2_var[treat2] <- rbinom(sum(treat2), 1, 1 - x2.fuzzy[1])
        t2_var[control2] <- rbinom(sum(control2), 1, x2.fuzzy[2])
      } else {
        stop("Fuzzy probability must between (>=) 0 and (<=) 1.")
      }
    }

    t_var <- t1_var | t2_var

    # } else {
    #   stop("cutpoint is not in the range of assignment variable")
    # }
    
    if (eta.sq > 0.99 || eta.sq < 0.01) {
      stop("eta.sq must between (>=) 0.01 and (<=) 0.99.")
    }
    
    # generate outcome before noise
    y_out <- coeff[1] + coeff[2] * t1_var + coeff[3] * t2_var + coeff[4] * t_var + 
      coeff[5] * x1_var + coeff[6] * x2_var + coeff[7] * x1_var * x2_var + 
      coeff[8] * t1_var * x1_var + coeff[9] * t2_var * x1_var + 
      coeff[10] * t1_var * x2_var + coeff[11] * t2_var * x2_var + 
      coeff[12] * t1_var * x1_var * x2_var + coeff[13] * t2_var * x1_var * x2_var
    
    if (x1.dist == "normal") {
      E_X1 <- x1.para[1]
      Var_X1 <- x1.para[2]^2
      E_T1 <- (1 - x1.fuzzy[1]) * 
        pnorm(x1.cut, mean = x1.para[1], sd = x1.para[2], lower.tail = FALSE) + 
        x1.fuzzy[2] * pnorm(x1.cut, mean = x1.para[1], sd = x1.para[2], lower.tail = TRUE)
      Var_T1 <- E_T1 - E_T1^2

    } else if (x1.dist == "uniform") {
      E_X1 <- (x1.para[1] + x1.para[2]) / 2
      Var_X1 <- (x1.para[1] - x1.para[2])^2 / 12
      E_T1 <- (1 - x1.fuzzy[1]) * (x1.para[2] - x1.cut) / (x1.para[2] - x1.para[1]) + 
        x1.fuzzy[2] * (x1.cut - x1.para[1]) / (x1.para[2] - x1.para[1])
      Var_T1 <- E_T1 - E_T1^2

    } else {
      stop("Distribution of 1st assignment variable X1 is not valid.")
    }

    if (x2.dist == "normal") {
      E_X2 <- x2.para[1]
      Var_X2 <- x2.para[2]^2
      E_T2 <- (1 - x2.fuzzy[1]) * 
        pnorm(x2.cut, mean = x2.para[1], sd = x2.para[2], lower.tail = FALSE) + 
        x2.fuzzy[2] * pnorm(x2.cut, mean = x2.para[1], sd = x2.para[2], lower.tail = TRUE)
      Var_T2 <- E_T2 - E_T2^2

    } else if (x2.dist == "uniform") {
      E_X2 <- (x2.para[1] + x2.para[2]) / 2
      Var_X2 <- (x2.para[1] - x2.para[2])^2 / 12
      E_T2 <- (1 - x2.fuzzy[1]) * (x2.para[2] - x2.cut) / (x2.para[2] - x2.para[1]) + 
        x2.fuzzy[2] * (x2.cut - x2.para[1]) / (x2.para[2] - x2.para[1])
      Var_T2 <- E_T2 - E_T2^2

    } else {
      stop("Distribution of 2nd assignment variable X2 is not valid.")
    }

    E_T <- E_T1 + E_T2 - E_T1 * E_T2
    Var_T <- E_T - E_T^2

    Var_noise <- Var_T * (1 / eta.sq - 1)

    # generate outcome after noise
    if (Var_noise >= 0) {
      y_out <- y_out + rnorm(sample.size, 0, sqrt(Var_noise))
    } else {
      stop("Partial eta-squared is not in an appropriate range.")
    }

    # lm_model <- lm(y_out ~ t_var+x_var+t_var:x_var)
    # print(summary(lm_model))
    
    mrd_model <- try(
      if (x1.fuzzy[1] == 0 && x1.fuzzy[2] == 0 && x2.fuzzy[1] == 0 && x2.fuzzy[2] == 0) {
        # sharp design
        mrd_est(y_out ~ x1_var + x2_var, cutpoint = c(x1.cut, x2.cut), 
          less = TRUE, method = c("center", "univ"), t.design = c(x1.design, x2.design))
      } else {
        # fuzzy design
        mrd_est(y_out ~ x1_var + x2_var + t_var, cutpoint = c(x1.cut, x2.cut), 
          less = TRUE, method = c("center", "univ"), t.design = c(x1.design, x2.design))
      }
    )
    
    if (class(mrd_model) == "try-error") {
      est_res[i, ] <- NA
      pval_res[i, ] <- NA
    } else {
      est_res[i, ] <- c(mrd_model[["center"]]$tau_MRD$est,
                        mrd_model[["univ"]]$tau_R$est,
                        mrd_model[["univ"]]$tau_M$est)
      pval_res[i, ] <- c(mrd_model[["center"]]$tau_MRD$p,
                        mrd_model[["univ"]]$tau_R$p,
                        mrd_model[["univ"]]$tau_M$p)
    }

  }
  
  comb_res <- matrix(NA, 6, 3 + length(alpha.list))
  comb_res[, 1] <- colSums(!is.na(est_res))
  comb_res[, 2] <- colMeans(est_res, na.rm = TRUE)
  comb_res[, 3] <- apply(est_res, 2, var, na.rm = TRUE)

  for (i in 1:length(alpha.list)) {
    alpha <- alpha.list[i]
    comb_res[, 3 + i] <- colMeans(pval_res < alpha, na.rm = TRUE) 
  }
  
  rownames(comb_res) <- rep(c("Linear", "Opt"), 3)
  colnames(comb_res) <- c("success", "mean(est)", "var(est)", as.character(alpha.list))
  
  return(comb_res)  
}







