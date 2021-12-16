#' Multivariate Regression Discontinuity Estimation
#' 
#' \code{mrd_est} estimates treatment effects in a multivariate regression discontinuity design (MRDD) with two assignment variables, 
#' including the frontier average treatment effect (\code{tau_MRD}) 
#' and frontier-specific effects (\code{tau_R} and \code{tau_M}) simultaneously. 
#' 
#' @param formula The formula of the MRDD; a symbolic description of the model to be fitted. This is supplied in the
#'   format of \code{y ~ x1 + x2} for a simple sharp MRDD or \code{y ~ x1 + x2 | c1 + c2}
#'   for a sharp MRDD with two covariates. A fuzzy MRDD may be specified as
#'   \code{y ~ x1 + x2 + z} where \code{x} is the running variable, and 
#'   \code{z} is the endogenous treatment variable. Covariates are then included in the 
#'   same manner as in a sharp MRDD.
#' @param data An optional data frame containing the variables in the model. If not found in \code{data},
#'   the variables are taken from \code{environment(formula)}. 
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param cutpoint A numeric vector of length 2 containing the cutpoints at which assignment to the treatment is determined. The default is c(0, 0).
#' @param bw A vector specifying the bandwidths at which to estimate the RD. 
#'   Possible values are \code{"IK09"}, \code{"IK12"}, and a user-specified non-negative numeric vector specifying the bandwidths at which to estimate the RD.
#'   The default is \code{"IK12"}. If \code{bw} is \code{"IK12"}, the bandwidth is calculated using the Imbens-Kalyanaraman 
#'   2012 method. If \code{bw}  is \code{"IK09"}, the bandwidth is calculated using 
#'   the Imbens-Kalyanaraman 2009 method. Then, the RD is estimated
#'   with that bandwidth, half that bandwidth, and twice that bandwidth. 
#'   If only a single value is passed into the function,
#'   the RD will similarly be estimated at that bandwidth, half that bandwidth, 
#'   and twice that bandwidth.
#' @param front.bw A non-negative numeric vector specifying the bandwidths at which to estimate the RD for each
#'   of three effects models. If \code{NA}, \code{front.bw} will be determined by cross-validation. The default is \code{NA}.
#' @param m A non-negative integer specifying the number of uniformly-at-random samples to draw as search candidates for \code{front.bw},
#'   if \code{front.bw} is \code{NA}. The default is 10.
#' @param k A non-negative integer specifying the number of folds for cross-validation to determine \code{front.bw},
#'   if \code{front.bw} is \code{NA}. The default is 5.
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.
#' @param se.type This specifies the robust standard error calculation method to use. Options are,
#'   as in \code{\link{vcovHC}}, \code{"HC3"}, \code{"const"}, \code{"HC"}, \code{"HC0"}, 
#'   \code{"HC1"}, \code{"HC2"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}. This option 
#'   is overridden by \code{cluster}.
#' @param cluster An optional vector of length n specifying clusters within which the errors are assumed
#'   to be correlated. This will result in reporting cluster robust SEs. This option overrides
#'   anything specified in \code{se.type}. It is suggested that data with a discrete running 
#'   variable be clustered by each unique value of the running variable (Lee and Card, 2008).
#' @param verbose A logical value indicating whether to print additional information to 
#'   the terminal. The default is \code{FALSE}.
#' @param less Logical. If \code{TRUE}, return the estimates of linear and optimal. If \code{FALSE} 
#'   return the estimates of linear, quadratic, cubic, optimal, half and double. The default is \code{FALSE}.
#' @param est.cov Logical. If \code{TRUE}, the estimates of covariates will be included.
#'   If \code{FALSE}, the estimates of covariates will not be included. The default is \code{FALSE}. This option is not
#'   applicable if method is \code{"front"}.
#' @param est.itt Logical. If \code{TRUE}, the estimates of ITT will be returned.
#'   If \code{FALSE}, the estimates of ITT will not be returned. The default is \code{FALSE}. This option is not
#'   applicable if method is \code{"front"}.
#' @param local A non-negative numeric vector of length 1 or 2 specifying the range of neighboring points around the cutoff on the 
#'   standardized scale, for each assignment variable. The default is 0.15. 
#' @param ngrid A non-negative integer specifying the number of non-zero grid points on each assignment variable,
#'   which is also the number of zero grid points on each assignment variable. The default is 250. The value used in 
#'   Wong, Steiner and Cook (2013) is 2500, which may cause long computational time.
#' @param margin A non-negative numeric vector of length 1 or 2 specifying the range of grid points beyond the minimum and maximum
#'   of sample points on each assignment variable. The default is 0.03.
#' @param boot An optional non-negative integer specifying the number of bootstrap samples to obtain standard error of estimates.
#'   This arugment is not optional if method is \code{"front"}.
#' @param method A string specifying the method to estimate the RD effect. Options are \code{"center"}, 
#'   \code{"univ"}, \code{"front"}.
#' @param t.design A character vector of length 2 specifying the treatment option according to design.
#'   The first entry is for \code{x1} and the second entry is for \code{x2}. Options are  
#'   \code{"g"} (treatment is assigned if \code{x1} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x1} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x1} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x1} is less than or equal to its cutoff).
#'   The same options are available for \code{x2}.
#' @param stop.on.error A logical value indicating whether to remove bootstraps which cause error in the \code{integrate} function. If \code{TRUE}, bootstraps which cause error are removed
#'   and resampled until the specified number of 
#'   bootstrap samples are acquired. If \code{FALSE}, bootstraps which cause error are not removed. The default is \code{TRUE}.
#'
#' @return \code{mrd_est} returns an object of \link{class} "\code{mrd}".
#'   The function \code{summary} is used to obtain and print a summary of the 
#'   estimated regression discontinuity. The object of class \code{mrd} is a list 
#'   containing the following components for each estimated treatment effect,
#'   \code{tau_MRD} or \code{tau_R} and \code{tau_M}:
#' \item{type}{A string denoting either \code{"sharp"} or \code{"fuzzy"} RDD.}
#' \item{call}{The matched call.}
#' \item{est}{Numeric vector of the estimate of the discontinuity in the outcome under 
#'   a sharp MRDD or the Wald estimator in the fuzzy MRDD, for each corresponding bandwidth.}
#' \item{se}{Numeric vector of the standard error for each corresponding bandwidth.}
#' \item{ci}{The matrix of the 95% confidence interval, \code{c("CI Lower Bound", "CI Upper Bound")} 
#'   for each corresponding bandwidth.}
#' \item{bw}{Numeric vector of each bandwidth used in estimation.}
#' \item{z}{Numeric vector of the z statistic for each corresponding bandwidth.}
#' \item{p}{Numeric vector of the p-value for each corresponding bandwidth.}
#' \item{obs}{Vector of the number of observations within the corresponding bandwidth.}
#' \item{cov}{The names of covariates.}
#' \item{model}{For a sharp design, a list of the \code{lm} objects is returned.
#'   For a fuzzy design, a list of lists is returned, each with two elements: 
#'   \code{firststage}, the first stage \code{lm} object, and \code{iv}, the \code{ivreg} object. 
#'   A model is returned for each corresponding bandwidth.}
#' \item{frame}{Returns the model frame used in fitting.}
#' \item{na.action}{The observations removed from fitting due to missingness.}
#' \item{impute}{A logical value indicating whether multiple imputation is used or not.}
#' \item{d}{Numeric vector of the effect size (Cohen's d) for each estimate.}
#'
#' @references Wong, V. C., Steiner, P. M., Cook, T. D. (2013). 
#'   Analyzing regression-discontinuity designs with multiple assignment variables: 
#'   A comparative study of four estimation methods.
#'   Journal of Educational and Behavioral Statistics, 38(2), 107-141.
#'   \url{https://journals.sagepub.com/doi/10.3102/1076998611432172}. 
#' @references Imbens, G., Kalyanaraman, K. (2009). 
#'   Optimal bandwidth choice for the regression discontinuity estimator 
#'   (Working Paper No. 14726). National Bureau of Economic Research.
#'   \url{https://www.nber.org/papers/w14726}.
#' @references Imbens, G., Kalyanaraman, K. (2012). 
#'   Optimal bandwidth choice for the regression discontinuity estimator. 
#'   The Review of Economic Studies, 79(3), 933-959.
#'   \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
#' @references Lee, D. S., Card, D. (2010).
#'   Regression discontinuity inference with specification error. 
#'   Journal of Econometrics, 142(2), 655-674. 
#'   \doi{10.1016/j.jeconom.2007.05.003}.
#' @references Lee, D. S., Lemieux, T. (2010).
#'   Regression Discontinuity Designs in Economics.
#'   Journal of Economic Literature, 48(2), 281-355. 
#'   \doi{10.1257/jel.48.2.281}.
#'
#' @importFrom Formula as.Formula
#' @importFrom stats model.frame na.pass complete.cases as.formula
#'
#' @include rd_est.R
#' @include var_center.R
#' @include mfrd_est.R
#' @include treat_assign.R
#'
#' @export
#' 
#' @examples
#' set.seed(12345)
#' x1 <- runif(1000, -1, 1)
#' x2 <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * (x1 >= 0) + 3 * cov + 10 * (x2 >= 0) + rnorm(1000)
#' # centering
#' mrd_est(y ~ x1 + x2 | cov, method = "center", t.design = c("geq", "geq"))
#' # univariate
#' mrd_est(y ~ x1 + x2 | cov, method = "univ", t.design = c("geq", "geq"))
#' # frontier
#' mrd_est(y ~ x1 + x2 | cov, method = "front", t.design = c("geq", "geq"))

mrd_est <- function(formula, data, subset = NULL, cutpoint = NULL, bw = NULL, 
  front.bw = NA, m = 10, k = 5,
  kernel = "triangular", se.type = "HC1", cluster = NULL, verbose = FALSE, 
  less = FALSE, est.cov = FALSE, est.itt = FALSE, local = 0.15, ngrid = 250, 
  margin = 0.03, boot = NULL, method = c("center", "univ", "front"), 
  t.design = NULL, stop.on.error = TRUE) {

  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  call <- match.call()
  
  # if data is not specified, look for variables in the formula from the global environment
  if (missing(data)) 
    data <- environment(formula)
  formula <- as.Formula(formula)
  X1 <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[1]]
  X2 <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[2]]
  Y <- model.frame(formula, rhs = 0, lhs = NULL, data = data, na.action = na.pass)[[1]]
  
  # if only a subset of data is needed for the model
  if (!is.null(subset)) {
    X1 <- X1[subset]
    X2 <- X2[subset]
    Y <- Y[subset]
    if (!is.null(cluster)) 
      cluster <- cluster[subset]
  }

  na.ok <- complete.cases(X1) & complete.cases(X2) & complete.cases(Y)
  
  # if another variable is provided in addition to x1 and x2, it will be considered as z
  if (length(all.vars(formula(formula, rhs = 1, lhs = FALSE))) > 2) {
    type <- "fuzzy"
    Z <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[3]]
    if (!is.null(subset)) 
      Z <- Z[subset]
    na.ok <- na.ok & complete.cases(Z)
    # if more than one variable is provided in addition to x1 and x2, it is redundant
    if (length(all.vars(formula(formula, rhs = 1, lhs = FALSE))) > 3) 
      stop("Invalid formula. Read ?rd_est for proper syntax.")
  } else {
    type <- "sharp"
  }
  
  covs <- NULL
  
  # if variables are provided after the first part of the formula, 
  # they will be considered as covariates
  if (length(formula)[2] > 1) {
    covs <- model.frame(formula, rhs = 2, lhs = 0, data = data, na.action = na.pass)
    if (!is.null(subset)) 
      covs <- subset(covs, subset)
    na.ok <- na.ok & complete.cases(covs)
    covs <- subset(covs, na.ok)
  }
  X1 <- X1[na.ok]
  X2 <- X2[na.ok]
  Y <- Y[na.ok]
  if (type == "fuzzy") 
    Z <- as.double(Z[na.ok])
  if (!is.null(cluster)) 
    cluster <- cluster[na.ok]
  
  if (is.null(cutpoint)) {
    cutpoint <- c(0, 0)
    if (verbose) 
      cat("No cutpoint provided. Using default cutpoint of c(0, 0).\n")
  }
  
  # if (frame) {
  #   if (type == "sharp") {
  #     if (!is.null(covs)) 
  #       dat.out <- data.frame(X1, X2, Y, covs) else dat.out <- data.frame(X1, X2, Y)
  #   } else {
  #     if (!is.null(covs)) 
  #       dat.out <- data.frame(X1, X2, Y, Z, covs) else dat.out <- data.frame(X1, X2, Y, Z)
  #   }
  # }
  
  o <- list()
  class(o) <- "mrd"
  
  # o$call <- call
  
  if (!all(t.design %in% c("g", "geq", "l", "leq"))) {
    stop("treatment design must be one of 'g', 'geq', 'l', 'leq'.")
  }
  
  # centering
  if ("center" %in% method) {
    # obtain the univariate assignment variable from the centering approach
    # design of treatment comes into play
    X <- var_center(cbind(X1, X2), cut = cutpoint, t.design = t.design, t.plot = FALSE)
    
    if (type == "sharp") {
      if (!is.null(covs)) {
        data <- data.frame(Y, X, covs)
        form <- as.Formula(paste("Y~X|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
      } else {
        data <- data.frame(Y, X)
        form <- as.formula(Y ~ X)
      }
    } else {
      if (!is.null(covs)) {
        data <- data.frame(Y, X, Z, covs)
        form <- as.Formula(paste("Y~X+Z|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
      } else {
        data <- data.frame(Y, X, Z)
        form <- as.formula(Y ~ X + Z)
      }
    }
    
    if (all(t.design %in% c("geq", "leq"))) {
      o[["center"]] <- list(tau_MRD = 
          eval(bquote(
            rd_est(formula = .(form), data = data, subset = NULL, cutpoint = 0, bw = .(bw), 
              kernel = .(kernel), se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = "leq")
          ))
      )
    } else {
      o[["center"]] <- list(tau_MRD =
          eval(bquote(
            rd_est(formula = .(form), data = data, subset = NULL, cutpoint = 0, bw = .(bw), 
              kernel = .(kernel), se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = "l")
          ))
        
      )
    }
    
  } 
  
  # univariate
  if ("univ" %in% method) {
    # design of treatment comes into play
    subset1 <- !as.logical(treat_assign(X2, cutpoint[2], t.design[2]))

    # design of treatment comes into play
    subset2 <- !as.logical(treat_assign(X1, cutpoint[1], t.design[1]))

    if (type == "sharp") {
      if (!is.null(covs)) {
        data <- data.frame(Y, X1, X2, covs)
        form1 <- as.Formula(paste("Y~X1|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
        form2 <- as.Formula(paste("Y~X2|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
      } else {
        data <- data.frame(Y, X1, X2)
        form1 <- as.formula(Y ~ X1)
        form2 <- as.formula(Y ~ X2)
      }
    } else {
      if (!is.null(covs)) {
        data <- data.frame(Y, X1, X2, Z, covs)
        form1 <- as.Formula(paste("Y~X1+Z|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
        form2 <- as.Formula(paste("Y~X2+Z|", paste(names(covs), collapse = "+", sep = ""), sep = ""))
      } else {
        data <- data.frame(Y, X1, X2, Z)
        form1 <- as.formula(Y ~ X1 + Z)
        form2 <- as.formula(Y ~ X2 + Z)
      }
    }
    
    if (!is.null(bw)) {
      if (length(bw) == 1 && is.numeric(bw)) {
        o[["univ"]] <- list(
          tau_R = eval(bquote(
            rd_est(formula = .(form1), data = data, subset = subset1, 
              cutpoint = .(cutpoint[1]), bw = .(bw), kernel = .(kernel), 
              se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[1]))
          )),
          tau_M = eval(bquote(
            rd_est(formula = .(form2), data = data, subset = subset2, 
              cutpoint = .(cutpoint[2]), bw = .(bw), kernel = .(kernel), 
              se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[2]))
          ))
        )
      } else if (length(bw) == 2 && is.numeric(bw)) {
        o[["univ"]] <- list(
          tau_R = eval(bquote(
            rd_est(formula = .(form1), data = data, subset = subset1, 
              cutpoint = .(cutpoint[1]), bw = .(bw[1]), kernel = .(kernel), 
              se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[1]))
          )),
          tau_M = eval(bquote(
            rd_est(formula = .(form2), data = data, subset = subset2, 
              cutpoint = .(cutpoint[2]), bw = .(bw[2]), kernel = .(kernel), 
              se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
              less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[2]))
          ))
        )
      } else {
        stop("Invalid bandwidth. Read ?mrd_est for proper syntax.")
      }
      
    } else {
      o[["univ"]] <- list(
        tau_R = eval(bquote(
          rd_est(formula = .(form1), data = data, subset = subset1, cutpoint = .(cutpoint[1]), 
            bw = NULL, kernel = .(kernel), se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
            less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[1]))
        )),  
        tau_M = eval(bquote(
          rd_est(formula = .(form2), data = data, subset = subset2, cutpoint = .(cutpoint[2]), 
            bw = NULL, kernel = .(kernel), se.type = .(se.type), cluster = cluster, verbose = .(verbose), 
            less = .(less), est.cov = .(est.cov), est.itt = .(est.itt), t.design = .(t.design[2]))  
        ))
      )
    }
    
  } 
  
  # frontier
  if ("front" %in% method) {
    o[["front"]] <- list(tau_MRD = 
        eval(bquote(
          mfrd_est(y = Y, x1 = X1, x2 = X2, c1 = .(cutpoint[1]), c2 = .(cutpoint[2]), 
            t.design = .(t.design), local = .(local), 
            front.bw = .(front.bw), m = .(m), k = .(k), kernel = .(kernel),
            ngrid = .(ngrid), margin = .(margin), boot = .(boot), cluster = .(cluster), stop.on.error = .(stop.on.error))
        ))
    )
  } 
  
  o$call <- call
  return(o)
}


