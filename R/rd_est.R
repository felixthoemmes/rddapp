#' Regression Discontinuity Estimation
#' 
#' \code{rd_est} estimates both sharp and fuzzy RDD, using parametric and non-parametric 
#' (local linear) models. 
#' It is based on the RDestimate function in the rdd package.
#' Sharp RDDs (both parametric and non-parametric) are estimated using \code{lm} in the 
#' \pkg{stats} package.
#' Fuzzy RDDs (both parametric and non-parametric) are estimated using two-stage least-squares 
#' \code{ivreg} in the \pkg{AER} package. 
#' For non-parametric models, Imbens-Kalyanaraman optimal bandwidths can be used, 
#  with both 2009 version \code{\link{bw_ik09}} and 2012 version \code{\link{bw_ik12}}. 
#'  
#' @param formula The formula of the RDD. This is supplied in the
#'   format of \code{y ~ x} for a simple sharp RDD, or \code{y ~ x | c1 + c2}
#'   for a sharp RDD with two covariates. Fuzzy RDD may be specified as
#'   \code{y ~ x + z} where \code{x} is the running variable, and 
#'   \code{z} is the endogenous treatment variable. Covariates are then included in the 
#'   same manner as in a sharp RDD.
#' @param data An optional data frame.
#' @param subset An optional vector specifying a subset of observations to be used
#' @param cutpoint The cutpoint. If omitted, it is assumed to be 0.
#' @param bw A numeric vector specifying the bandwidths at which to estimate the RD. 
#'   If omitted or it is \code{"IK12"}, the bandwidth is calculated using the Imbens-Kalyanaraman 
#'   2012 method. If it is \code{"IK09"}, the bandwidth is calculated using 
#'   the Imbens-Kalyanaraman 2009 method. Then it is estimated
#'   with that bandwidth, half that bandwidth, and twice that bandwidth.  
#'   If only a single value is passed into the function,
#'   the RD will similarly be estimated at that bandwidth, half that bandwidth, 
#'   and twice that bandwidth.
#' @param kernel A string specifying the kernel to be used in the local linear fitting. 
#'   \code{"triangular"} kernel is the default and is the "correct" theoretical kernel to be 
#'   used for edge estimation as in RDD (Lee and Lemieux, 2010). Other options are 
#'   \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, \code{"gaussian"} and \code{"cosine"}.
#' @param se.type This specifies the robust SE calculation method to use. Options are,
#'   as in \code{\link{vcovHC}}, \code{"HC3"}, \code{"const"}, \code{"HC"}, \code{"HC0"}, 
#'   \code{"HC1"}, \code{"HC2"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}. This option 
#'   is overridden by \code{cluster}.
#' @param cluster An optional vector specifying clusters within which the errors are assumed
#'   to be correlated. This will result in reporting cluster robust SEs. This option overrides
#'   anything specified in \code{se.type}. It is suggested that data with a discrete running 
#'   variable be clustered by each unique value of the running variable (Lee and Card, 2008).
#' @param verbose Will provide some additional information printed to the terminal.
#' @param less Logical. If \code{TRUE}, return the estimates of linear and optimal, 
#'   instead of linear, quadratic, cubic, optimal, half and double.
#' @param est.cov Logical. If \code{TRUE}, the estimates of covariates will be included.
#' @param est.itt Logical. If \code{TRUE}, the estimates of ITT will be returned.
#' @param t.design The treatment option according to design.
#'   The entry is for X: \code{"g"} means treatment is assigned 
#'   if X is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if X is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if X is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if X is less than or equal to its cutoff.
#'
#' @return \code{rd_est} returns an object of \link{class} "\code{rd}".
#'   The functions \code{summary} and \code{plot} are used to obtain and print a summary and 
#'   plot of the estimated regression discontinuity. The object of class \code{rd} is a list 
#'   containing the following components:
#' \item{type}{A string denoting either \code{"sharp"} or \code{"fuzzy"} RDD.}
#' \item{est}{Numeric vector of the estimate of the discontinuity in the outcome under 
#'   a sharp design, or the Wald estimator in the fuzzy design for each corresponding bandwidth.}
#' \item{se}{Numeric vector of the standard error for each corresponding bandwidth.}
#' \item{z}{Numeric vector of the z statistic for each corresponding bandwidth.}
#' \item{p}{Numeric vector of the p value for each corresponding bandwidth.}
#' \item{ci}{The matrix of the 95% confidence interval, \code{c("CI Lower Bound", "CI Upper Bound")} 
#'   for each corresponding bandwidth.}
#' \item{d}{Numeric vector of the effective size (Cohen's d) for each estimate.}
#' \item{cov}{The names of covariates.}
#' \item{bw}{Numeric vector of each bandwidth used in estimation.}
#' \item{obs}{Vector of the number of observations within the corresponding bandwidth.}
#' \item{call}{The matched call.}
#' \item{na.action}{The observations removed from fitting due to missingness.}
#' \item{impute}{Whether multiple imputation is used or not.}
#' \item{model}{For a sharp design, a list of the \code{lm} objects is returned.
#'   For a fuzzy design, a list of lists is returned, each with two elements: 
#'   \code{firststage}, the first stage \code{lm} object, and \code{iv}, the \code{ivreg} object. 
#'   A model is returned for each corresponding bandwidth.}
#' \item{frame}{Returns the model frame used in fitting.}
#'
#' @references Lee, D. S., Lemieux, T. (2010).
#'   Regression Discontinuity Designs in Economics.
#'   Journal of Economic Literature, 48(2), 281-355. 
#'   \url{http://www.aeaweb.org/articles.php?doi=10.1257/jel.48.2.281}.
#' @references Imbens, G., Lemieux, T. (2008).
#'   Regression discontinuity designs: A guide to practice.
#'   Journal of Econometrics, 142(2), 615-635. 
#'   \url{http://dx.doi.org/10.1016/j.jeconom.2007.05.001}.
#' @references Lee, D. S., Card, D. (2010).
#'   Regression discontinuity inference with specification error. 
#'   Journal of Econometrics, 142(2), 655-674. 
#'   \url{http://dx.doi.org/10.1016/j.jeconom.2007.05.003}.
#' @references Angrist, J. D., Pischke, J.-S. (2009). 
#'   Mostly harmless econometrics: An empiricist's companion. 
#'   Princeton, NJ: Princeton University Press.
#'
#' @importFrom AER ivreg 
#' @importFrom sandwich estfun sandwich vcovHC
#' @importFrom lmtest coeftest
#' @importFrom Formula as.Formula
#' @importFrom stats model.frame na.pass complete.cases lm coef pnorm qnorm as.formula
#'
#' @include bw_ik12.R
#' @include bw_ik09.R 
#' @include wt_kern.R
#' @include treat_assign.R
#'
#' @export
#'
#' @examples
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' rd_est(y ~ x, t.design = "geq")
#' # Efficiency gains can be made by including covariates
#' rd_est(y ~ x | cov, t.design = "geq")

rd_est <- function(formula, data, subset = NULL, cutpoint = NULL, bw = NULL, 
  kernel = "triangular", se.type = "HC1", cluster = NULL, verbose = FALSE, less = FALSE, 
  est.cov = FALSE, est.itt = FALSE, t.design = NULL) {
  
  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  call <- match.call()
  
  if (missing(data)) 
    data <- environment(formula)
  formula <- as.Formula(formula)
  X <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[1]]
  Y <- model.frame(formula, rhs = 0, lhs = NULL, data = data, na.action = na.pass)[[1]]

  # if only a subset of data is needed for the model
  if (!is.null(subset)) {
    X <- X[subset]
    Y <- Y[subset]
    if (!is.null(cluster)) 
      cluster <- cluster[subset]
  }

  # if data is clustered, a clustered estimator of covariance is needed
  if (!is.null(cluster)) {
    cluster <- as.character(cluster)
    robust.se <- function(model, cluster) {
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- model$rank
      dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
      uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
      rcse.cov <- dfc * sandwich(model, meat. = crossprod(uj) / N)
      rcse.se <- coeftest(model, rcse.cov)
      return(rcse.se)
    }
  }

  na.ok <- complete.cases(X) & complete.cases(Y)
  
  # if another variable is provided in addition to x, it will be considered as z
  if (length(all.vars(formula(formula, rhs = 1, lhs = FALSE))) > 1) {
    type <- "fuzzy"
    Z <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[2]]
    if (!is.null(subset)) 
      Z <- Z[subset]
    na.ok <- na.ok & complete.cases(Z)
    # if more than one variable is provided in addition to x1 and x2, it is redundant
    if (length(all.vars(formula(formula, rhs = 1, lhs = FALSE))) > 2) 
      stop("Invalid formula. Read ?rd_est for proper syntax.")
  } else {
    type <- "sharp"
  }

  covs <- NULL
  num_covs <- 0
  
  # if variables are provided after the first part of the formula, 
  # they will be considered as covariates
  if (length(formula)[2] > 1) {
    covs <- model.frame(formula, rhs = 2, lhs = 0, data = data, na.action = na.pass)
    if (!is.null(subset)) 
      covs <- subset(covs, subset)
    na.ok <- na.ok & complete.cases(covs)
    covs <- subset(covs, na.ok)
    num_covs <- ncol(covs)
  }

  X <- X[na.ok]
  Y <- Y[na.ok]

  if (type == "fuzzy") 
    Z <- as.double(Z[na.ok])
  
  if (is.null(cutpoint)) {
    cutpoint <- 0
    if (verbose) 
      cat("No cutpoint provided. Using default cutpoint of zero.\n")
  }

  if (type == "sharp") {
    if (!is.null(covs)) 
      dat.out <- data.frame(X, Y, covs) 
    else dat.out <- data.frame(X, Y)
  } else {
    if (!is.null(covs)) 
      dat.out <- data.frame(X, Y, Z, covs) 
    else dat.out <- data.frame(X, Y, Z)
  }

  if (is.null(bw) || bw == "IK12") {
    bw <- try(bw_ik12(X = X, Y = Y, cutpoint = cutpoint, kernel = kernel, verbose = verbose), 
      silent = TRUE)
    if (class(bw) == "try-error") {
      bws <- c(NA, NA, NA, -1, -1, -1)
      warning("Fail to calculate the IK12 bandwidth, nonparametric models will be NA.")
    } else {
      bws <- c(NA, NA, NA, bw, 0.5 * bw, 2 * bw)  
    }
    names(bws) <- c("Linear", "Quadratic", "Cubic", "Opt", "Half-Opt", "Double-Opt")
  } else if (bw == "IK09") {
    bw <- try(bw_ik09(X = X, Y = Y, cutpoint = cutpoint, kernel = kernel, verbose = verbose), 
      silent = TRUE)
    if (class(bw) == "try-error") {
      bws <- c(NA, NA, NA, -1, -1, -1)
      warning("Fail to calculate the IK09 bandwidth, nonparametric models will be NA.")
    } else {
      bws <- c(NA, NA, NA, bw, 0.5 * bw, 2 * bw)
    }
    names(bws) <- c("Linear", "Quadratic", "Cubic", "Opt", "Half-Opt", "Double-Opt")
  } else if (length(bw) == 1 && is.numeric(bw)) {
    bws <- c(NA, NA, NA, bw, 0.5 * bw, 2 * bw)
    names(bws) <- c("Linear", "Quadratic", "Cubic", "Usr", "Half-Usr", "Double-Usr")
  } else {
    stop("Invalid bandwidth. Read ?rd_est for proper syntax.")  
  }
  
  # if only linear and local linear models are needed
  if (less) {
    bws <- bws[c(1, 4)]
  }
  
  # Setup values to be returned
  o <- list()
  class(o) <- "rd"

  o$type <- type
  o$call <- call
  if (est.cov) {
    o$est <- vector(length = length(bws) * (1 + num_covs), mode = "numeric")
    names(o$est) <- rep(names(bws), each = 1 + num_covs)
    o$se <- vector(length = length(bws) * (1 + num_covs), mode = "numeric")
    if (type == "sharp") {
      names(o$se) <- rep(c("Tr", names(covs)), length(bws))
    } else {
      names(o$se) <- rep(c("Z", names(covs)), length(bws))
    }
    o$ci <- matrix(NA, nrow = length(bws) * (1 + num_covs), ncol = 2)
  } else {
    o$est <- vector(length = length(bws), mode = "numeric")
    names(o$est) <- names(bws)
    o$se <- vector(length = length(bws), mode = "numeric")
    if (type == "sharp") {
      names(o$se) <- rep("Tr", length(bws))
    } else {
      names(o$se) <- rep("Z", length(bws))
    }
    o$ci <- matrix(NA, nrow = length(bws), ncol = 2)
  }
  o$bw <- bws
  o$z <- vector(mode = "numeric")
  o$p <- vector(mode = "numeric")
  o$obs <- vector(mode = "numeric")
  o$cov <- names(covs)
  o$model <- list()
  if (type == "fuzzy") {
    o$model$firststage <- list()
    o$model$iv <- list()
  }
  o$frame <- dat.out
  o$na.action <- which(na.ok == FALSE)
  o$impute <- FALSE
  
  X <- X - cutpoint
  Tr <- treat_assign(X, 0, t.design)
  Xl <- (1 - Tr) * X
  Xr <- Tr * X

  if (type == "fuzzy" && est.itt) {
    Z <- Tr
  }

  degree <- c(1, 2, 3)
  for (ibw in 1:length(bws)) {
    bw <- bws[ibw]

    if (est.cov) {
      pos <- (1 + (ibw - 1) * (1 + num_covs)):(ibw * (1 + num_covs))
    } else {
      pos <- ibw
    }
    
    if (!is.na(bw) && bw <= 0) {
      o$obs[ibw] <- NA
      o$est[pos] <- NA
      o$se[pos] <- NA
      o$z[pos] <- NA
      o$p[pos] <- NA
      o$ci[pos, ] <- NA
      o$model[[ibw]] <- NA
    } else {
      # ibw <- which(bw == bws) 
      # Subset to within the bandwidth, except for when using gaussian weighting
      # sub <- X >= (-bw) & X <= (+bw)
      
      # if (kernel == "gaussian") 
      #   sub <- TRUE
      
      if (is.na(bw)) {
        o$obs[ibw] <- length(X)
      } else {
        w <- wt_kern(X, 0, bw, kernel = kernel)
        o$obs[ibw] <- sum(w > 0)
      }
      
      if (type == "sharp") {
        if (verbose) {
          cat("Running Sharp RD\n")
          cat("Running variable:", all.vars(formula(formula, rhs = 1, lhs = FALSE))[1], "\n")
          cat("Outcome variable:", all.vars(formula(formula, rhs = FALSE, lhs = 1))[1], "\n")
          if (!is.null(covs)) 
            cat("Covariates:", paste(names(covs), collapse = ", "), "\n")
        }
        if (!is.null(covs)) {
          if (is.na(bw)) {
            data <- data.frame(Y, Tr, Xl, Xr, covs)
            form <- as.formula(paste("Y ~ Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE) + ", 
              paste(names(covs), collapse = "+", sep = ""), sep = ""))
          } else {
            data <- data.frame(Y, Tr, Xl, Xr, covs, w)
            form <- as.formula(paste("Y ~ Tr + Xl + Xr + ", 
              paste(names(covs), collapse = "+", sep = ""), sep = ""))
          }
        } else {
          if (is.na(bw)) {
            data <- data.frame(Y, Tr, Xl, Xr)
            form <- as.formula(paste("Y ~ Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE)", sep = ""))
          } else {
            data <- data.frame(Y, Tr, Xl, Xr, w)
            form <- as.formula(Y ~ Tr + Xl + Xr)
          }
        }
        
        if (is.na(bw)) {
          mod <- try(lm(form, data = data), silent = TRUE)
        } else {
          mod <- try(lm(form, weights = w, data = subset(data, w > 0)), silent = TRUE)
        }
        
        if (class(mod) == "try-error") {
          o$est[pos] <- NA
          o$se[pos] <- NA
          o$z[pos] <- NA
          o$p[pos] <- NA
          o$ci[pos, ] <- NA
          o$model[[ibw]] <- NA
        } else {
          if (verbose == TRUE) {
            cat("Model:\n")
            print(summary(mod))
          }
          
          if (est.cov) {
            o$est[pos] <- coef(mod)[c("Tr", names(covs))]
          } else {
            o$est[pos] <- coef(mod)["Tr"]
          }
          
          if (is.null(cluster)) {
            test_tab <- coeftest(mod, vcovHC(mod, type = se.type))
          } else {
            if (is.na(bw)) {
              test_tab <- robust.se(mod, cluster[na.ok])
            } else {
              test_tab <- robust.se(mod, cluster[na.ok][w > 0])
            }
          }
          
          if (est.cov) {
            o$se[pos] <- rep(NA, 1 + num_covs)
            test_var <- intersect(c("Tr", names(covs)), rownames(test_tab))
            o$se[pos][test_var] <- test_tab[test_var, 2]
          } else {
            o$se[pos] <- ifelse("Tr" %in% rownames(test_tab), test_tab["Tr", 2], NA)
          }
          
          o$z[pos] <- o$est[pos] / o$se[pos]
          o$p[pos] <- 2 * pnorm(abs(o$z[pos]), lower.tail = FALSE)
          o$ci[pos, ] <- c(o$est[pos] - qnorm(0.975) * o$se[pos], 
            o$est[pos] + qnorm(0.975) * o$se[pos])
          
          o$model[[ibw]] <- mod
        }
        
      } else {
        if (verbose) {
          cat("Running Fuzzy RD\n")
          cat("Running variable:", all.vars(formula(formula, rhs = 1, lhs = FALSE))[1], "\n")
          cat("Outcome variable:", all.vars(formula(formula, rhs = FALSE, lhs = 1))[1], "\n")
          cat("Treatment variable:", all.vars(formula(formula, rhs = 1, lhs = FALSE))[2], "\n")
          if (!is.null(covs)) 
            cat("Covariates:", paste(names(covs), collapse = ", "), "\n")
        }
        
        if (!is.null(covs)) {
          if (is.na(bw)) {
            data <- data.frame(Y, Tr, Xl, Xr, Z, covs)
            form <- as.Formula(paste("Y ~ Z + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE) + ", 
              paste(names(covs), collapse = "+"), "|Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE) + ", 
              paste(names(covs), collapse = "+"), sep = ""))
            form1 <- as.Formula(paste("Z ~ Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE) + ", 
              paste(names(covs), collapse = "+", sep = "")))
          } else {
            data <- data.frame(Y, Tr, Xl, Xr, Z, covs, w)
            form <- as.Formula(paste("Y ~ Z + Xl + Xr + ", paste(names(covs), collapse = "+"), 
              "|Tr + Xl + Xr + ", paste(names(covs), collapse = "+"), sep = ""))
            form1 <- as.Formula(paste("Z ~ Tr + Xl + Xr + ", paste(names(covs), collapse = "+", 
              sep = "")))
          }
        } else {
          if (is.na(bw)) {
            data <- data.frame(Y, Tr, Xl, Xr, Z)
            form <- as.Formula(paste("Y ~ Z + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], 
              ", raw = TRUE) | Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE)", sep = ""))
            form1 <- as.formula(paste("Z ~ Tr + poly(Xl, ", degree[ibw], 
              ", raw = TRUE) + poly(Xr, ", degree[ibw], ", raw = TRUE)", sep = ""))
          } else {
            data <- data.frame(Y, Tr, Xl, Xr, Z, w)
            form <- as.Formula(Y ~ Z + Xl + Xr | Tr + Xl + Xr)
            form1 <- as.formula(Z ~ Tr + Xl + Xr)
          }
        }
        
        if (is.na(bw)) {
          mod1 <- try(lm(form1, data = data), silent = TRUE)
          mod <- try(ivreg(form, data = data), silent = TRUE)
        } else {
          mod1 <- try(lm(form1, weights = w, data = subset(data, w > 0)), silent = TRUE)
          mod <- try(ivreg(form, weights = w, data = subset(data, w > 0)), silent = TRUE)
        }
        
        if (class(mod) == "try-error") {
          o$est[pos] <- NA
          o$se[pos] <- NA
          o$z[pos] <- NA
          o$p[pos] <- NA
          o$ci[pos, ] <- NA
          o$model$firststage[[ibw]] <- NA
          o$model$iv[[ibw]] <- NA
        } else {
          if (verbose == TRUE) {
            cat("First stage:\n")
            print(summary(mod1))
            cat("IV-RD:\n")
            print(summary(mod))
          }
          
          if (est.cov) {
            o$est[pos] <- coef(mod)[c("Z", names(covs))]
          } else {
            o$est[pos] <- coef(mod)["Z"]
          }
          
          if (is.null(cluster)) {
            test_tab <- coeftest(mod, vcovHC(mod, type = se.type))
          } else {
            if (is.na(bw)) {
              test_tab <- robust.se(mod, cluster[na.ok])
            } else {
              test_tab <- robust.se(mod, cluster[na.ok][w > 0])
            }
          }
          
          if (est.cov) {
            o$se[pos] <- rep(NA, 1 + num_covs)
            test_var <- intersect(c("Z", names(covs)), rownames(test_tab))
            o$se[pos][test_var] <- test_tab[test_var, 2]
          } else {
            o$se[pos] <- ifelse("Z" %in% rownames(test_tab), test_tab["Z", 2], NA)
          }
          
          o$z[pos] <- o$est[pos] / o$se[pos]
          o$p[pos] <- 2 * pnorm(abs(o$z[pos]), lower.tail = FALSE)
          o$ci[pos, ] <- c(o$est[pos] - qnorm(0.975) * o$se[pos], 
            o$est[pos] + qnorm(0.975) * o$se[pos])
          
          o$model$firststage[[ibw]] <- mod1
          o$model$iv[[ibw]] <- mod
        }
      }
      
      if (est.cov && num_covs > 0){
        d = o$d <- o$est / sd(Y)
        d[seq(2, length(d), 2)] = NA
        o$d <- d
      }else{
        o$d <- o$est / sd(Y)
      }

    }
  }
  
  names(o$se) <- names(o$est)
  
  return(o)
} 
