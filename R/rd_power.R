#' Power Analysis of Regression Discontinuity
#' 
#' \code{rd_power} computes the empirical probability that a resulting parameter
#' estimate of the MRD is significant,
#' i.e. the empirical power (1 - beta).
#'
#' @param num.rep A non-negative integer specifying the number of repetitions used to calculate the empirical power. The default is 100.
#' @param sample.size A non-negative integer specifying the number of observations in each sample. The default is 100.
#' @param x.dist A string specifying the distribution of the assignment variable, \code{x}.
#'   Options are \code{"normal"} and  \code{"uniform"}. The default is the \code{"normal"} distribution.
#' @param x.para A numeric vector of length 2 specifying parameters of the distribution of the first assignment variable, \code{x1}.
#'   If \code{x.dist} is \code{"normal"}, then \code{x.para} includes the
#'   mean and standard deviation of the normal distribution.
#'   If \code{x.dist} is \code{"uniform"}, then \code{x.para} includes the 
#'   upper and lower boundaries of the uniform distribution. The default is c(0,1).
#' @param x.cut A numeric value containing the cutpoint at which assignment to the treatment is determined. The default is 0.
#' @param x.fuzzy A numeric vector of length 2 specifying the probabilities to be assigned to the control, in terms of the
#'   assignment variable, \code{x}, for individuals in the treatment based on the cutoff, 
#'   and to treatment for individuals in the control based on the cutoff.
#'   For a sharp design, both entries are 0. 
#'   For a fuzzy design, the first entry is the probability to be assigned to 
#'   control for individuals above the cutpoint, and the second entry is the 
#'   probability to be assigned to treatment for individuals below the cutpoint.
#'   The default is c(0,0), indicating a sharp design. 
#' @param x.design A string specifying the treatment option according to design.
#'   Options are \code{"g"} (treatment is assigned if \code{x} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x} is less than or equal to its cutoff).
#' @param coeff A numeric vector specifying coefficients of variables in the linear model to generate data.
#'   Coefficients are in the following order:
#'   \itemize{
#'   \item{The 1st entry is the intercept.} 
#'   \item{The 2nd entry is the slope of treatment, i.e. treatment effect.}
#'   \item{The 3rd entry is the slope of assignment.}
#'   \item{The 4th entry is the slope of interaction between treatment and assignment.}
#'   }
#'   The default is c(0.3, 1, 0.2, 0.3).
#' @param eta.sq A numeric value specifying the expected partial eta-squared of the linear model with respect to the 
#'   treatment itself. It is used to control the variance of noise in the linear model. The default is 0.50. 
#' @param alpha.list A numeric vector containing significance levels (between 0 and 1) used to calculate the empirical alpha.
#'   The default is c(0.001, 0.01, anad 0.05).
#'
#' @return \code{rd_power} returns an object of \link{class} 
#'   "\code{rdp}", including containing the mean, variance, and power (with \code{alpha} of 0.001, 0.01, and 0.05)
#'   for two estimators. The function \code{summary}
#'   is used to obtain and print a summary of the power analysis. The two estimators are:
#'   \itemize{
#'   \item{The 1st estimator, \code{Linear}, provides results of the linear regression estimator.} 
#'   \item{The 2nd estimator, \code{Opt}, provides results of the local linear regression estimator of RD,
#'         with the optimal bandwidth in the Imbens and Kalyanaraman (2012) paper.}
#'   }
#'
#' @references Imbens, G., Kalyanaraman, K. (2012). 
#'   Optimal bandwidth choice for the regression discontinuity estimator. 
#'   The Review of Economic Studies, 79(3), 933-959.
#'   \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
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
#' summary(rd_power(x.design = "l"))
#' summary(rd_power(x.dist = "uniform", x.cut = 0.5, x.design = "l"))
#' summary(rd_power(x.fuzzy = c(0.1, 0.1), x.design = "l"))
#' }

rd_power <- function(num.rep = 100, sample.size = 100, x.dist = "normal", x.para = c(0, 1), 
  x.cut = 0, x.fuzzy = c(0, 0), x.design = NULL, coeff = c(0.3, 1, 0.2, 0.3), eta.sq = 0.5, 
  alpha.list = c(0.001, 0.01, 0.05)) {
  est_res <- matrix(NA, num.rep, 2)
  pval_res <- matrix(NA, num.rep, 2)
  
  if (is.null(x.design)){
    stop("Specify x.design.")
  }

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
      rd_est(y_out ~ x_var, cutpoint = x.cut, less = TRUE, t.design = x.design)
    } else {
      # fuzzy design
      rd_est(y_out ~ x_var + t_var, cutpoint = x.cut, less = TRUE, t.design = x.design)
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
  
  class(comb_res) <- "rdp"
  
  return(comb_res)  
}







