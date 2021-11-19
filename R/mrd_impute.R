#' Multiple Imputation of Multivariate Regression Discontinuity Estimation
#' 
#' \code{mrd_impute} estimates treatment effects in a multivariate regression discontinuity design (MRDD) with imputed missing values. 
#' 
#' @param formula The formula of the MRDD; a symbolic description of the model to be fitted. This is supplied in the
#'   format of \code{y ~ x1 + x2} for a simple sharp MRDD, or \code{y ~ x1 + x2 | c1 + c2}
#'   for a sharp MRDD with two covariates. Fuzzy MRDD may be specified as
#'   \code{y ~ x1 + x2 + z} where \code{x} is the running variable, and 
#'   \code{z} is the endogenous treatment variable. Covariates are then included in the 
#'   same manner as in a sharp MRDD.
#' @param data An optional data frame containing the variables in the model. If not found in \code{data},
#'   the variables are taken from \code{environment(formula)}. 
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param cutpoint An optional numeric vector of length 2 containing the cutpoints at which assignment to the treatment is determined. The default is c(0, 0).
#' @param bw A vector specifying the bandwidths at which to estimate the RD. 
#'   Possible values are \code{"IK09"}, \code{"IK12"}, and a user-specified non-negative numeric vector specifying the bandwidths at which to estimate the RD.
#'   The default is \code{"IK12"}. If \code{bw} is \code{"IK12"}, the bandwidth is calculated using the Imbens-Kalyanaraman 
#'   2012 method. If \code{bw}  is \code{"IK09"}, the bandwidth is calculated using 
#'   the Imbens-Kalyanaraman 2009 method. Then the RD is estimated
#'   with that bandwidth, half that bandwidth, and twice that bandwidth. 
#'   If only a single value is passed into the function,
#'   the RD will similarly be estimated at that bandwidth, half that bandwidth, 
#'   and twice that bandwidth.
#' @param front.bw An optional non-negative numeric vector specifying the bandwidths at which to estimate the RD for each
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
#' @param local A non-negative numeric vector of length 1 or 2 specifying the range of neighboring points around the cutoff on the 
#'   standardized scale, for each assignment variable. The default is 0.15. 
#' @param ngrid A non-negative integer specifying the number of non-zero grid points on each assignment variable,
#'   which is also the number of zero grid points on each assignment variable. The default is 250. The value used in 
#'   Wong, Steiner and Cook (2013) is 2500, which may cause long computational time.
#' @param margin A non-negative numeric vector of length 1 or 2 specifying the range of grid points beyond the minimum and maximum
#'   of sample points on each assignment variable. The default is 0.03.
#' @param boot An optional non-negative integer specifying the number of bootstrap samples to obtain standard error of estimates.
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
#' @return \code{mrd_impute} returns an object of \link{class} "\code{mrd}", or "\code{mrdi}" for
#' \code{"front"} method.
#'
#' @references Wong, V. C., Steiner, P. M., Cook, T. D. (2013). 
#'   Analyzing regression-discontinuity designs with multiple assignment variables: 
#'   A comparative study of four estimation methods.
#'   Journal of Educational and Behavioral Statistics, 38(2), 107-141.
#'   \url{https://journals.sagepub.com/doi/10.3102/1076998611432172}.
#' @references Lee, D. S., Lemieux, T. (2010).
#'   Regression Discontinuity Designs in Economics.
#'   Journal of Economic Literature, 48(2), 281-355. 
#'   \doi{10.1257/jel.48.2.281}.
#' @references Lee, D. S., Card, D. (2010).
#'   Regression discontinuity inference with specification error. 
#'   Journal of Econometrics, 142(2), 655-674. 
#'   \doi{10.1016/j.jeconom.2007.05.003}.
#'
#' @importFrom Formula as.Formula
#' @importFrom stats complete.cases pt qt
#'
#' @include mrd_est.R
#'
#' @export
#'
#' @examples
#' set.seed(12345)
#' x1 <- runif(300, -1, 1)
#' x2 <- runif(300, -1, 1)
#' cov <- rnorm(300)
#' y <- 3 + 2 * (x1 >= 0) + 3 * cov + 10 * (x2 >= 0) + rnorm(300)
#' imp <- rep(1:3, each = 100)
#' # all examples below have smaller numbers of m to keep run-time low
#' # centering
#' mrd_impute(y ~ x1 + x2 | cov, impute = imp, method = "center", t.design = c("geq", "geq"), m = 3)
#' # univariate
#' mrd_impute(y ~ x1 + x2 | cov, impute = imp, method = "univ", t.design = c("geq", "geq"), m = 3)
#' # frontier
#' mrd_impute(y ~ x1 + x2 | cov, impute = imp, method = "front", t.design = c("geq", "geq"), m = 3)

mrd_impute <- function(formula, data, subset = NULL, cutpoint = NULL, bw = NULL, 
  front.bw = NA, m = 10, k = 5,
  kernel = "triangular", se.type = "HC1", cluster = NULL, impute = NULL, verbose = FALSE, 
  less = FALSE, est.cov = FALSE, est.itt = FALSE, local = 0.15, ngrid = 250, margin = 0.03, 
  boot = NULL, method = c("center", "univ", "front"), t.design = NULL, stop.on.error = TRUE) {
 
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
  class(o) <- "mrd"

  o$call <- call
  
  est_res <- list()
  d_res <- list()
  se_res <- list()
  #name_mod <- list()

  for (i in 1:num_imp) {
    imp_sub <- na.ok & (impute == imp_list[i])

    if (is.null(subset)) {
      if (missing(data)) {
        curr_mod <- mrd_est(formula = formula, subset = imp_sub, cutpoint = cutpoint, bw = bw,
          front.bw = front.bw, m = m, k = k,
          kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, less = less, 
          est.cov = est.cov, est.itt = est.itt, local = local, ngrid = ngrid, margin = margin, 
          boot = boot, method = method, t.design = t.design, 
          stop.on.error = stop.on.error)
      } else {
        curr_mod <- mrd_est(formula = formula, data = data, subset = imp_sub, cutpoint = cutpoint, 
          bw = bw, front.bw = front.bw, m = m, k = k,
          kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, 
          less = less, est.cov = est.cov, est.itt = est.itt, local = local, ngrid = ngrid, 
          margin = margin, boot = boot, method = method, t.design = t.design, 
          stop.on.error = stop.on.error)
      }
      
    } else {
      if (missing(data)) {
        curr_mod <- mrd_est(formula = formula, subset = (subset & imp_sub), cutpoint = cutpoint, 
          bw = bw, front.bw = front.bw, m = m, k = k,
          kernel = kernel, se.type = se.type, cluster = cluster, verbose = verbose, 
          less = less, est.cov = est.cov, est.itt = est.itt, local = local, ngrid = ngrid, 
          margin = margin, boot = boot, method = method, t.design = t.design,
          stop.on.error = stop.on.error)
      } else {
        curr_mod <- mrd_est(formula = formula, data = data, subset = (subset & imp_sub), 
          cutpoint = cutpoint, bw = bw, front.bw = front.bw, m = m, k = k,
          kernel = kernel, se.type = se.type, cluster = cluster,
          verbose = verbose, less = less, est.cov = est.cov, est.itt = est.itt, local = local, 
          ngrid = ngrid, margin = margin, boot = boot, method = method, t.design = t.design,
          stop.on.error = stop.on.error)
      }
      
    }
    
    if ("center" %in% method) {
      center_MRD <- curr_mod$center$tau_MRD

      if (i == 1) {
        o$center$tau_MRD$call <- center_MRD$call
        o$center$tau_MRD$type <- center_MRD$type
        o$center$tau_MRD$cov <- center_MRD$cov
        o$center$tau_MRD$bw <- center_MRD$bw
        o$center$tau_MRD$obs <- center_MRD$obs
        o$center$tau_MRD$model <- center_MRD$model
        o$center$tau_MRD$frame <- center_MRD$frame
        o$center$tau_MRD$na.action <- center_MRD$na.action

        num_covs <- length(center_MRD$cov)
        num_mod <- ifelse(less, 2, 6)
        num_est <- ifelse(est.cov, 1 + num_covs, 1)

        est_res$center_MRD <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(est_res$center_MRD) <- names(center_MRD$est)
        d_res$center_MRD <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(d_res$center_MRD) <- names(center_MRD$d)
        se_res$center_MRD <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(se_res$center_MRD) <- names(center_MRD$se)
      }

      est_res$center_MRD[i, ] <- center_MRD$est
      d_res$center_MRD[i, ] <- center_MRD$d
      se_res$center_MRD[i, ] <- center_MRD$se
    }

    if ("univ" %in% method) {
      univ_R <- curr_mod$univ$tau_R
      univ_M <- curr_mod$univ$tau_M

      if (i == 1) {
        # R
        o$univ$tau_R$call <- univ_R$call
        o$univ$tau_R$type <- univ_R$type
        o$univ$tau_R$cov <- univ_R$cov
        o$univ$tau_R$bw <- univ_R$bw
        o$univ$tau_R$obs <- univ_R$obs
        o$univ$tau_R$model <- univ_R$model
        o$univ$tau_R$frame <- univ_R$frame
        o$univ$tau_R$na.action <- univ_R$na.action

        num_covs <- length(univ_R$cov)
        num_mod <- ifelse(less, 2, 6)
        num_est <- ifelse(est.cov, 1 + num_covs, 1)

        est_res$univ_R <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(est_res$univ_R) <- names(univ_R$est)
        d_res$univ_R <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(d_res$univ_R) <- names(univ_R$d)
        se_res$univ_R <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(se_res$univ_R) <- names(univ_R$se)

        # M
        o$univ$tau_M$call <- univ_M$call
        o$univ$tau_M$type <- univ_M$type
        o$univ$tau_M$cov <- univ_M$cov
        o$univ$tau_M$bw <- univ_M$bw
        o$univ$tau_M$obs <- univ_M$obs
        o$univ$tau_M$model <- univ_M$model
        o$univ$tau_M$frame <- univ_M$frame
        o$univ$tau_M$na.action <- univ_M$na.action
        
        num_covs <- length(univ_M$cov)
        num_mod <- ifelse(less, 2, 6)
        num_est <- ifelse(est.cov, 1 + num_covs, 1)

        est_res$univ_M <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(est_res$univ_M) <- names(univ_M$est)
        d_res$univ_M <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(d_res$univ_M) <- names(univ_M$d)
        se_res$univ_M <- matrix(NA, nrow = num_imp, ncol = num_mod * num_est)
        colnames(se_res$univ_M) <- names(univ_M$se)
      }

      est_res$univ_R[i, ] <- univ_R$est
      d_res$univ_R[i, ] <- univ_R$d
      se_res$univ_R[i, ] <- univ_R$se

      est_res$univ_M[i, ] <- univ_M$est
      d_res$univ_M[i, ] <- univ_M$d
      se_res$univ_M[i, ] <- univ_M$se
    }

    if ("front" %in% method) {
      front_MRD <- curr_mod$front$tau_MRD

      if (i == 1) {
        o$front$tau_MRD$call <- front_MRD$call
        
        M <- matrix(NA, nrow = num_imp, ncol = 9)
        colnames(M) <- colnames(front_MRD$est)
        est_res$front_MRD <- list('Param' = M, 'bw' = M, 'Half-bw' = M, 'Double-bw' = M)
        d_res$front_MRD <- list('Param' = M, 'bw' = M, 'Half-bw' = M, 'Double-bw' = M)
        se_res$front_MRD <- list('Param' = M, 'bw' = M, 'Half-bw' = M, 'Double-bw' = M)
      }

      est_res$front_MRD$'Param'[i, ] <- front_MRD$est['Param',]
      est_res$front_MRD$'bw'[i, ] <- front_MRD$est['bw',]
      est_res$front_MRD$'Half-bw'[i, ] <- front_MRD$est['Half-bw',]
      est_res$front_MRD$'Double-bw'[i, ] <- front_MRD$est['Double-bw',]
      d_res$front_MRD$'Param'[i, ] <- front_MRD$d['Param',]
      d_res$front_MRD$'bw'[i, ] <- front_MRD$d['bw',]
      d_res$front_MRD$'Half-bw'[i, ] <- front_MRD$d['Half-bw',]
      d_res$front_MRD$'Double-bw'[i, ] <- front_MRD$d['Double-bw',]      
      se_res$front_MRD$'Param'[i, ] <- front_MRD$se['Param',]
      se_res$front_MRD$'bw'[i, ] <- front_MRD$se['bw',]
      se_res$front_MRD$'Half-bw'[i, ] <- front_MRD$se['Half-bw',]
      se_res$front_MRD$'Double-bw'[i, ] <- front_MRD$se['Double-bw',]
    }

  }
  
  if ("center" %in% method) {
    Q <- colMeans(est_res$center_MRD)
    D <- colMeans(d_res$center_MRD)
    U <- colMeans(se_res$center_MRD)
    B <- apply(se_res$center_MRD, 2, var)
    V <- U + (1 + 1 / num_imp) * B
    
    o$center$tau_MRD$est <- Q
    o$center$tau_MRD$d <- D
    o$center$tau_MRD$se <- sqrt(V)
    o$center$tau_MRD$z <- unname(o$center$tau_MRD$est/o$center$tau_MRD$se)
    
    r <- (1 + 1 / num_imp) * B / U
    df <- (num_imp - 1) * (1 + 1 / r)^2
    o$center$tau_MRD$df <- df
    
    o$center$tau_MRD$p <- 2 * pt(abs(o$center$tau_MRD$z), df = df, lower.tail = FALSE)
    o$center$tau_MRD$ci <- unname(c(
      o$center$tau_MRD$est - qt(0.975, df = df) * o$center$tau_MRD$se, 
      o$center$tau_MRD$est + qt(0.975, df = df) * o$center$tau_MRD$se))
    o$center$tau_MRD$ci <- matrix(o$center$tau_MRD$ci, ncol = 2)
    
    #o$center$tau_MRD$bw <- rep(NA, length(name_mod$center_MRD))
    #names(o$center$tau_MRD$bw) <- name_mod$center_MRD
    #o$center$tau_MRD$obs <- rep(NA, length(name_mod$center_MRD))   

    o$center$tau_MRD$impute <- TRUE
    class(o$center$tau_MRD) <- "rd"                 
  }

  if ("univ" %in% method) {
    # R
    Q <- colMeans(est_res$univ_R)
    D <- colMeans(d_res$univ_R)
    U <- colMeans(se_res$univ_R)
    B <- apply(se_res$univ_R, 2, var)
    V <- U + (1 + 1 / num_imp) * B
    
    o$univ$tau_R$est <- Q
    o$univ$tau_R$d <- D
    o$univ$tau_R$se <- sqrt(V)
    o$univ$tau_R$z <- unname(o$univ$tau_R$est/o$univ$tau_R$se)
    
    r <- (1 + 1 / num_imp) * B / U
    df <- (num_imp - 1) * (1 + 1 / r)^2
    o$univ$tau_R$df <- df
    
    o$univ$tau_R$p <- 2 * pt(abs(o$univ$tau_R$z), df = df, lower.tail = FALSE)
    o$univ$tau_R$ci <- unname(c(
      o$univ$tau_R$est - qt(0.975, df = df) * o$univ$tau_R$se, 
      o$univ$tau_R$est + qt(0.975, df = df) * o$univ$tau_R$se))
    o$univ$tau_R$ci <- matrix(o$univ$tau_R$ci, ncol = 2)
    
    #o$univ$tau_R$bw <- rep(NA, length(name_mod$univ_R))
    #names(o$univ$tau_R$bw) <- name_mod$univ_R
    #o$univ$tau_R$obs <- rep(NA, length(name_mod$univ_R))   

    o$univ$tau_R$impute <- TRUE
    class(o$univ$tau_R) <- "rd"

    # M
    Q <- colMeans(est_res$univ_M)
    D <- colMeans(d_res$univ_M)
    U <- colMeans(se_res$univ_M)
    B <- apply(se_res$univ_M, 2, var)
    V <- U + (1 + 1 / num_imp) * B
    
    o$univ$tau_M$est <- Q
    o$univ$tau_M$d <- D
    o$univ$tau_M$se <- sqrt(V)
    o$univ$tau_M$z <- unname(o$univ$tau_M$est/o$univ$tau_M$se)
    
    r <- (1 + 1 / num_imp) * B / U
    df <- (num_imp - 1) * (1 + 1 / r)^2
    o$univ$tau_M$df <- df
    
    o$univ$tau_M$p <- 2 * pt(abs(o$univ$tau_M$z), df = df, lower.tail = FALSE)
    o$univ$tau_M$ci <- unname(c(
      o$univ$tau_M$est - qt(0.975, df = df) * o$univ$tau_M$se, 
      o$univ$tau_M$est + qt(0.975, df = df) * o$univ$tau_M$se))
    o$univ$tau_M$ci <- matrix(o$univ$tau_M$ci, ncol = 2)
    
    #o$univ$tau_M$bw <- rep(NA, length(name_mod$univ_M))
    #names(o$univ$tau_M$bw) <- name_mod$univ_M
    #o$univ$tau_M$obs <- rep(NA, length(name_mod$univ_M))       

    o$univ$tau_M$impute <- TRUE
    class(o$univ$tau_M) <- "rd"      
  }
  
  if ("front" %in% method) {
    Q <- rbind(colMeans(est_res$front_MRD$'Param'), colMeans(est_res$front_MRD$'bw'),
               colMeans(est_res$front_MRD$'Half-bw'), colMeans(est_res$front_MRD$'Double-bw'))
    D <- rbind(colMeans(d_res$front_MRD$'Param'), colMeans(d_res$front_MRD$'bw'),
               colMeans(d_res$front_MRD$'Half-bw'), colMeans(d_res$front_MRD$'Double-bw'))
    U <- rbind(colMeans(se_res$front_MRD$'Param'), colMeans(se_res$front_MRD$'bw'),
               colMeans(se_res$front_MRD$'Half-bw'), colMeans(se_res$front_MRD$'Double-bw'))
    B <- rbind(apply(se_res$front_MRD$'Param', 2, var), apply(se_res$front_MRD$'bw', 2, var),
               apply(se_res$front_MRD$'Half-bw', 2, var), apply(se_res$front_MRD$'Double-bw', 2, var))
    V <- U + (1 + 1 / num_imp) * B
    list.names = c('Param', 'bw', 'Half-bw', 'Double-bw')
    rownames(Q) = rownames(D) = rownames(U) = rownames(B) = rownames(V) <- list.names
    
    o$front$tau_MRD$est <- Q
    o$front$tau_MRD$d <- D
    o$front$tau_MRD$se <- sqrt(V)
    o$front$tau_MRD$z <- o$front$tau_MRD$est/o$front$tau_MRD$se
    
    r <- (1 + 1 / num_imp) * B / U
    df <- (num_imp - 1) * (1 + 1 / r)^2
    o$front$tau_MRD$df <- df
    
    o$front$tau_MRD$p <- 2 * pt(abs(o$front$tau_MRD$z), df = df, lower.tail = FALSE)
    ci <- matrix(NA, 8, 9)
    ci[c(1, 3, 5, 7),] <- o$front$tau_MRD$est - qt(0.975, df = df) * o$front$tau_MRD$se
    ci[c(2, 4, 6, 8),] <- o$front$tau_MRD$est + qt(0.975, df = df) * o$front$tau_MRD$se
    rownames(ci) <- rep(c('2.5%', '97.5%'), 4)
    colnames(ci) <- colnames(o$front$tau_MRD$est)
    o$front$tau_MRD$ci <- ci
    
    o$front$tau_MRD$w <- front_MRD$w   
    o$front$tau_MRD$front.bw <- front_MRD$front.bw

    o$front$tau_MRD$impute <- TRUE
    class(o) = "mrdi"
    class(o$front$tau_MRD) <- "mfrdi"                
  }

  return(o)
}


