#' Multiple Imputation of Regression Discontinuity Estimation
#' 
#' \code{rd_impute} estimates treatment effects in a RDD with imputed missing values. 
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
#' @param impute An optional vector specifying the imputed variables with missing values. 
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
#' @return \code{rd_impute} returns an object of \link{class} "\code{rd}".
#'
#' @references Stata: 64 mi estimate - Estimation using multiple imputations
#'
#' @importFrom stats complete.cases pt qt
#'
#' @include rd_est.R
#'
#' @export
#'
#' @examples
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x < 0) + rnorm(1000)
#' group <- rep(1:10, each = 100)
#' rd_impute(y ~ x, impute = group)
#' # Efficiency gains can be made by including covariates
#' rd_impute(y ~ x | cov, impute = group)

rd_impute <- function(formula, data, subset = NULL, cutpoint = NULL, bw = NULL, 
  kernel = "triangular", se.type = "HC1", cluster = NULL, impute = NULL, verbose = FALSE, 
  less = FALSE, est.cov = FALSE, est.itt = FALSE, t.design = "l") {
  call <- match.call()

  impute <- as.character(impute)
  na.ok <- complete.cases(impute)
  imp_list <- unique(impute[na.ok])
  num_imp <- length(imp_list) 
  
  if (num_imp == 0) {
    stop("Invalid imputed variable with 0 category. Read ?rd_impute for proper syntax.")
  } else if (num_imp == 1) {
    stop("Invalid imputed variable with 1 category. Use ?rd_est instead.")
  }
  
  o <- list()
  class(o) <- "rd"

  o$call <- call
  o$impute <- TRUE

  for (i in 1:num_imp) {
    imp_sub <- na.ok & (impute == imp_list[i])
    
    if (is.null(subset)) {
      if (missing(data)) {
        curr_mod <- rd_est(formula = formula, subset = imp_sub, cutpoint = cutpoint, bw = bw,
          kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, less = less, 
          est.cov = est.cov, est.itt = est.itt, t.design = t.design)
      } else {
        curr_mod <- rd_est(formula = formula, data = data, subset = imp_sub, cutpoint = cutpoint, 
          bw = bw, kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, 
          less = less, est.cov = est.cov, est.itt = est.itt, t.design = t.design)
      }
      
    } else {
      if (missing(data)) {
        curr_mod <- rd_est(formula = formula, subset = (subset & imp_sub), cutpoint = cutpoint, 
          bw = bw, kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, 
          less = less, est.cov = est.cov, est.itt = est.itt, t.design = t.design)
      } else {
        curr_mod <- rd_est(formula = formula, data = data, subset = (subset & imp_sub), 
          cutpoint = cutpoint, bw = bw, kernel = kernel, se.type = se.type, cluster = cluster, 
          verbose = verbose, less = less, est.cov = est.cov, est.itt = est.itt, t.design = t.design)
      }
      
    }
    
    if (i == 1) {
      o$type <- curr_mod$type
      o$cov <- curr_mod$cov
      o$bw <- curr_mod$bw
      o$obs <- curr_mod$obs
      o$model <- curr_mod$model
      o$frame <- curr_mod$frame
      o$na.action <- curr_mod$na.action

      num_covs <- length(curr_mod$cov)
      num_mod <- ifelse(less, 2, 6)
      num_est <- ifelse(est.cov, 1 + num_covs, 1)

      est_res <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
      colnames(est_res) <- names(curr_mod$est)
      d_res <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
      colnames(d_res) <- names(curr_mod$d)
      se_res <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
      colnames(se_res) <- names(curr_mod$se)
    }

    est_res[i, ] <- curr_mod$est
    d_res[i, ] <- curr_mod$d
    se_res[i, ] <- curr_mod$se
  }

  Q <- colMeans(est_res)
  D <- colMeans(d_res)
  U <- colMeans(se_res)
  B <- apply(se_res, 2, var)
  V <- U + (1 + 1 / num_imp) * B
  
  o$est <- Q
  o$d <- D
  o$se <- sqrt(V)
  o$z <- unname(o$est/o$se)
  
  r <- (1 + 1 / num_imp) * B / U
  df <- (num_imp - 1) * (1 + 1 / r)^2
  o$df <- df
  
  o$p <- 2 * pt(abs(o$z), df = df, lower.tail = FALSE)
  o$ci <- unname(c(o$est - qt(0.975, df = df) * o$se, o$est + 
                     qt(0.975, df = df) * o$se))
  o$ci <- matrix(o$ci, ncol = 2)
  
  # o$bw <- rep(NA, length(name_mod))
  # names(o$bw) <- name_mod
  # o$obs <- rep(NA, length(name_mod))

  return(o)
}


