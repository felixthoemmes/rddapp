#' Multiple Imputation of Regression Discontinuity Estimation
#' 
#' \code{rd_impute} estimates treatment effects in an RDD with imputed missing values. 
#' 
#' @param formula The formula of the RDD; a symbolic description of the model to be fitted. This is supplied in the
#'   format of \code{y ~ x} for a simple sharp RDD or \code{y ~ x | c1 + c2}
#'   for a sharp RDD with two covariates. A fuzzy RDD may be specified as
#'   \code{y ~ x + z} where \code{x} is the running variable, and 
#'   \code{z} is the endogenous treatment variable. Covariates are included in the 
#'   same manner as in a sharp RDD.
#' @param data An optional data frame containing the variables in the model. If not found in \code{data},
#'   the variables are taken from \code{environment(formula)}.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param cutpoint A numeric value containing the cutpoint at which assignment to the treatment is determined. The default is 0.
#' @param bw A vector specifying the bandwidths at which to estimate the RD. 
#'   Possible values are \code{"IK09"}, \code{"IK12"}, and a user-specified non-negative numeric vector specifying the bandwidths at which to estimate the RD.
#'   The default is \code{"IK12"}. If \code{bw} is \code{"IK12"}, the bandwidth is calculated using the Imbens-Kalyanaraman 
#'   2012 method. If \code{bw}  is \code{"IK09"}, the bandwidth is calculated using 
#'   the Imbens-Kalyanaraman 2009 method. Then the RD is estimated
#'   with that bandwidth, half that bandwidth, and twice that bandwidth. 
#'   If only a single value is passed into the function,
#'   the RD will similarly be estimated at that bandwidth, half that bandwidth, 
#'   and twice that bandwidth.
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.
#' @param se.type This specifies the robust standard error calculation method to use. Options are,
#'   as in \code{\link{vcovHC}}, \code{"HC3"}, \code{"const"}, \code{"HC"}, \code{"HC0"}, 
#'   \code{"HC1"}, \code{"HC2"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}. This option 
#'   is overridden by \code{cluster}.
#' @param cluster An optional vector specifying clusters within which the errors are assumed
#'   to be correlated. This will result in reporting cluster robust SEs. This option overrides
#'   anything specified in \code{se.type}. It is suggested that data with a discrete running 
#'   variable be clustered by each unique value of the running variable (Lee and Card, 2008).
#' @param impute An optional vector specifying the imputed variables with missing values. 
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
#' @param t.design A string specifying the treatment option according to design.
#'   Options are \code{"g"} (treatment is assigned if \code{x} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x} is less than or equal to its cutoff).
#'
#' @return \code{rd_impute} returns an object of \link{class} "\code{rd}".
#'
#' @references Stata: 64 mi estimate - Estimation using multiple imputations
#' @references Lee, D. S., Card, D. (2010).
#'   Regression discontinuity inference with specification error. 
#'   Journal of Econometrics, 142(2), 655-674. 
#'   \doi{10.1016/j.jeconom.2007.05.003}.
#' @references Imbens, G., Kalyanaraman, K. (2009). 
#'   Optimal bandwidth choice for the regression discontinuity estimator 
#'   (Working Paper No. 14726). National Bureau of Economic Research.
#'   \url{https://www.nber.org/papers/w14726}.
#' @references Imbens, G., Kalyanaraman, K. (2012). 
#'   Optimal bandwidth choice for the regression discontinuity estimator. 
#'   The Review of Economic Studies, 79(3), 933-959.
#'   \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
#'
#' @importFrom stats complete.cases pt qt
#'
#' @include rd_est.R
#'
#' @export 
#'
#' @examples
#' set.seed(12345)
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x < 0) + rnorm(1000)
#' group <- rep(1:10, each = 100)
#' rd_impute(y ~ x, impute = group, t.design = "l")
#' # Efficiency gains can be made by including covariates
#' rd_impute(y ~ x | cov, impute = group, t.design = "l")

rd_impute <- function(formula, data, subset = NULL, cutpoint = NULL, bw = NULL, 
  kernel = "triangular", se.type = "HC1", cluster = NULL, impute = NULL, verbose = FALSE, 
  less = FALSE, est.cov = FALSE, est.itt = FALSE, t.design = NULL) {

  if (is.null(t.design)){
    stop("Specify t.design.")
  }

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


