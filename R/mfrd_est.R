#' Multivariate Frontier Regression Discontinuity Estimation 
#'
#' \code{mfrd_est} implements the frontier approach for multivariate regression discontinuity estimation in Wong, Steiner and Cook (2013). 
#' It is based on the MFRDD code in Stata from Wong, Steiner, and Cook (2013). 
#'
#' @param y A numeric object containing outcome variable.
#' @param x1 A numeric object containing the first assignment variable.
#' @param x2 A numeric object containing the second assignment variable.
#' @param c1 A numeric value containing the cutpoint at which assignment to the treatment is determined for \code{x1}.
#' @param c2 A numeric value containing the cutpoint at which assignment to the treatment is determined for \code{x2}.
#' @param t.design A character vector of length 2 specifying the treatment option according to design.
#'   The first entry is for \code{x1} and the second entry is for \code{x2}. Options are  
#'   \code{"g"} (treatment is assigned if \code{x1} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x1} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x1} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x1} is less than or equal to its cutoff).
#'   The same options are available for \code{x2}.
#' @param local A non-negative numeric value specifying the range of neighboring points around the cutoff on the 
#'   standardized scale, for each assignment variable. The default is 0.15. 
#' @param front.bw A non-negative numeric vector specifying the bandwidths at which to estimate the RD for each
#'   of three effects models. If \code{NA}, \code{front.bw} will be determined by cross-validation. The default is \code{NA}.
#' @param m A non-negative integer specifying the number of uniformly-at-random samples to draw as search candidates for \code{front.bw},
#'   if \code{front.bw} is \code{NA}. The default is 10.
#' @param k A non-negative integer specifying the number of folds for cross-validation to determine \code{front.bw},
#'   if \code{front.bw} is \code{NA}. The default is 5.
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.
#' @param ngrid A non-negative integer specifying the number of non-zero grid points on each assignment variable,
#'   which is also the number of zero grid points on each assignment variable. The default is 250. The value used in 
#'   Wong, Steiner and Cook (2013) is 2500, which may cause long computational time.
#' @param margin A non-negative numeric value specifying the range of grid points beyond the minimum and maximum
#'   of sample points on each assignment variable. The default is 0.03.
#' @param boot An optional non-negative integer specifying the number of bootstrap samples to obtain standard error of estimates.
#' @param cluster An optional vector specifying clusters within which the errors are assumed
#'   to be correlated. This will result in reporting cluster robust SEs. It is suggested that data with a discrete running 
#'   variable be clustered by each unique value of the running variable (Lee and Card, 2008).
#' @param stop.on.error A logical value indicating whether to remove bootstraps which cause error in the \code{integrate} function. If \code{TRUE}, bootstraps which cause error are removed
#'   and resampled until the specified number of 
#'   bootstrap samples are acquired. If \code{FALSE}, bootstraps which cause error are not removed. The default is \code{TRUE}.
#'
#' @return \code{mfrd_est} returns an object of \link{class} "\code{mfrd}".
#'   The functions \code{summary} and \code{plot} are used to obtain and print a summary and 
#'   plot of the estimated regression discontinuity. The object of class \code{mfrd} is a list 
#'   containing the following components:
#' \item{w}{Numeric vector specifying the weight of frontier 1 and frontier 2, respectively.}   
#' \item{est}{Numeric matrix of the estimate of the discontinuity in the outcome under 
#'   a complete model (no prefix), heterogeneous treatment (ht) effects model, and treatment (t) only model,
#'   for the parametric case and for each corresponding bandwidth.
#'   Estimates with suffix "ev1" and "ev2" correspond to expected values for each frontier, under a given model.
#'   Estimates with suffix "ate" correspond to average treatment effects across both frontiers, 
#'   under a given model.}
#' \item{d}{Numeric matrix of the effect size (Cohen's d) for estimate.}
#' \item{se}{Numeric matrix of the standard error for each corresponding bandwidth and ....}
#' \item{m_s}{A list containing estimates for the complete model, under parametric
#' and non-parametric (optimal, half, and double bandwidth) cases. A list of 
#' coefficient estimates, residuals, effects, weights (in the non-parametric case),
#' rank, fitted values, assignments,
#' qr, residual degrees of freedom, levels of the x value, function call, terms,
#' and output data frame are returned for each model.}
#' \item{m_h}{A list containing estimates for the heterogeneous treatments model, under parametric
#' and non-parametric (optimal, half, and double bandwidth) cases. A list of 
#' coefficient estimates, residuals, effects, weights (in the non-parametric case),
#' rank, fitted values, assignments,
#' qr, residual degrees of freedom, levels of the x value, function call, terms,
#' and output data frame are returned for each model.}
#' \item{m_t}{A list containing estimates for the treatment only model, under parametric
#' and non-parametric (optimal, half, and double bandwidth) cases. A list of 
#' coefficient estimates, residuals, effects, weights (in the non-parametric case),
#' rank, fitted values, assignments,
#' qr, residual degrees of freedom, levels of the x value, function call, terms,
#' and output data frame are returned for each model.}
#' \item{dat_h}{A list containing four data frames, one for each case:
#' parametric or non-parametric (optimal, half, and double bandwidth).
#' Each data frame contains ... colnames ... for each ... row information.}
#' \item{dat}{A data frame containing the outcome (y) and each input (x1, x2) for each observation.
#' The data frame also contains sample restrictions for x1 and x2 (x1res, x2res),
#' scaled (zx1, zx2) and centered x1 and x2 values (zcx1, zcx2), and 
#' treatment indicators for overall treatment (tr) based on treatment assignment
#' from x1 (tr1), x2 (tr2), and both assignment variables (trb).}
#' \item{obs}{List of the number of observations within the corresponding bandwidth.}
#' \item{impute}{A logical value indicating whether multiple imputation is used or not.}
#' \item{call}{The matched call.}
#' \item{front.bw}{Numeric vector of each bandwidth used in estimation.}
#' 
#' @references Wong, V., Steiner, P, and Cook, T. (2013).
#'   Analyzing regression discontinuity designs with multiple assignment variables: A comparative study of four estimation methods. 
#'   Journal of Educational and Behavioral Statistics, 38(2), 107-141. 
#'   \doi{10.3102/1076998611432172}.
#'   
#' @references Lee, D. and Card, D. (2008).
#'   A Regression discontinuity inference with specification error.
#'   Journal of Econometrics, 142(2), 655-674. 
#'   \doi{10.1016/j.jeconom.2007.05.003}.
#'
#' @importFrom stats bw.nrd0 integrate splinefun predict.lm
#'
#' @include treat_assign.R
#' @include wt_kern_bivariate.R
#'
#' @export
#'
#' @examples
#' set.seed(12345)
#' x1 <- runif(1000, -1, 1)
#' x2 <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * (x1 >= 0) + 3 * cov + 10 * (x2 >= 0) + rnorm(1000)
#' mfrd_est(y = y, x1 = x1, x2 = x2, c1 = 0, c2 = 0, t.design = c("geq", "geq"))

mfrd_est <- function(y, x1, x2, c1, c2, t.design = NULL, local = 0.15, front.bw = NA, m = 10, k = 5, kernel = "triangular",
                        ngrid = 250, margin = 0.03, boot = NULL, cluster = NULL, stop.on.error = TRUE) {
  
  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  if (any(!is.na(front.bw)) & length(front.bw)!=3){
    stop("front.bw is NA or a numeric vector of length 3.")
  }
  
  # parametric version
  call <- match.call()
  
  out <- mfrd_est_single(y = y, x1 = x1, x2 = x2, c1 = c1, c2 = c2, 
                         t.design = t.design, local = local, front.bw = NA, kernel = kernel,
                         ngrid = ngrid, margin = margin)
  # keep track of number of bad bootstrap samples
  badboot <- 0
  
  if (is.numeric(boot) && boot > 0) {
    est_boot <- matrix(NA, boot, 9)
    b = 1
    while (b <= boot) {
      if (is.null(cluster)) {
        index <- sample(length(y), replace = TRUE)
      } else {
        index <- clus_boot(length(y), cluster = cluster)
      }
      tryboot <- try(mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], c1 = c1, c2 = c2, 
                                     t.design = t.design, local = local, front.bw = NA, kernel = kernel,
                                     ngrid = ngrid, margin = margin, 
                                     stop.on.error = TRUE)$est, silent = TRUE)
      if (stop.on.error){
        if (inherits(tryboot, "try-error")){
          badboot <- badboot + 1
        }else{
          est_boot[b, ] <- tryboot
          b <- b + 1
        }
      }else{
        if (inherits(tryboot, "try-error")){
          badboot <- badboot + 1
          est_boot[b, ] <- mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], c1 = c1, c2 = c2, 
                                           t.design = t.design, local = local, front.bw = NA, kernel = kernel,
                                           ngrid = ngrid, margin = margin, 
                                           stop.on.error = stop.on.error)$est
        }else{
          est_boot[b, ] <- tryboot 
        }
        b <- b + 1
      }
      
    }
    out$est_boot = list(est_boot)
    se_boot <- apply(est_boot, 2, sd, na.rm = TRUE)
    ci_boot <- apply(est_boot, 2, quantile, na.rm = TRUE, probs = c(.025,.975))
    out$se <- se_boot
    out$ci <- ci_boot
  }
  
  out$m_s <- list(out$m_s)
  out$m_h <- list(out$m_h)
  out$m_t <- list(out$m_t)
  out$dat_h <- list(out$dat_h)
  out$impute <- FALSE
  out$call <- call
  out$obs <- list(out$obs)
  
  if (badboot>0){
    if (stop.on.error){
      cat(paste("Discarded and resampled ", badboot, " out of ", boot, " bootstrap samples (",
                format(badboot/boot*100, digits = 3),
                "%) which caused errors.\n", sep= ""))
    }else{
      cat(paste(badboot, " out of the ", boot, " bootstrap samples (", 
                format(badboot/boot*100, digits = 3),
                "%) caused errors.\n", sep= ""))
    }
  }
  
  # nonparametric version, do cross validation if front.bw not given
  if (all(!is.na(front.bw))){
    bw.opt <- front.bw
  }else{
    cv <- function(front.bw){return(mfrd_est_cv(y, x1, x2, c1, c2, t.design, local, front.bw, 0.25,
                                          k, kernel, ngrid, margin))}
    bw.seq <-  runif(m, min = 0.5, max = 2.5)
    mse.cv <- sapply(bw.seq, cv)
    min.idx <- as.numeric(apply(mse.cv, 1, which.min))
    bw.opt <- bw.seq[min.idx]
    # default of bandwidth is 1 when minimum cannot be found due to no points within testing bandwidth
    bw.opt[is.na(bw.opt)] = 1
  }
  
  # concatenate results for the bandwidth, and half and double the bandwidth
  bw.list = list(bw.opt, 0.5*bw.opt, 2*bw.opt)
  
  for (bw.vec in bw.list){
    out.nonparam <- mfrd_est_single(y = y, x1 = x1, x2 = x2, c1 = c1, c2 = c2, 
                                    t.design = t.design, local = local, front.bw = bw.vec, kernel = kernel,
                                    ngrid = ngrid, margin = margin)
    # keep track of number of bad bootstrap samples
    badboot <- 0
    
    if (is.numeric(boot) && boot > 0) {
      est_boot <- matrix(NA, boot, 9)
      b = 1
      while (b <= boot) {
        if (is.null(cluster)) {
          index <- sample(length(y), replace = TRUE)
        } else {
          index <- clus_boot(length(y), cluster = cluster)
        }
        tryboot <- try(mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], c1 = c1, c2 = c2, 
                                       t.design = t.design, local = local, front.bw = bw.vec, kernel = kernel,
                                       ngrid = ngrid, margin = margin, 
                                       stop.on.error = TRUE)$est, silent = TRUE)
        if (stop.on.error){
          if (inherits(tryboot, "try-error")){
            badboot <- badboot + 1
          }else{
            est_boot[b, ] <- tryboot
            b <- b + 1
          }
        }else{
          if (inherits(tryboot, "try-error")){
            badboot <- badboot + 1
            est_boot[b, ] <- mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], c1 = c1, c2 = c2, 
                                             t.design = t.design, local = local, front.bw = bw.vec, kernel = kernel,
                                             ngrid = ngrid, margin = margin, 
                                             stop.on.error = stop.on.error)$est
          }else{
            est_boot[b, ] <- tryboot 
          }
          b <- b + 1
        }
        
      }
      out$est_boot = c(out$est_boot, list(est_boot))
      se_boot <- apply(est_boot, 2, sd, na.rm = TRUE)
      ci_boot <- apply(est_boot, 2, quantile, na.rm = TRUE, probs = c(.025,.975))
      out$se <- rbind(out$se, se_boot)
      out$ci <- rbind(out$ci, ci_boot)
    }else{
      out$se <- rbind(out$se, out.nonparam$se)
    }
    
    out$est <- rbind(out$est, out.nonparam$est)
    out$d <- rbind(out$d, out.nonparam$d)
    out$m_s <- c(out$m_s, list(out.nonparam$m_s))
    out$m_h <- c(out$m_h, list(out.nonparam$m_h))
    out$m_t <- c(out$m_t, list(out.nonparam$m_t))
    out$dat_h <- c(out$dat_h, list(out.nonparam$dat_h))
    out$obs <- c(out$obs, list(out.nonparam$obs))
    
    if (badboot>0){
      if (stop.on.error){
        cat(paste("Discarded and resampled ", badboot, " out of ", boot, " bootstrap samples (",
                  format(badboot/boot*100, digits = 3),
                  "%) which caused errors.\n", sep= ""))
      }else{
        cat(paste(badboot, " out of the ", boot, " bootstrap samples (", 
                  format(badboot/boot*100, digits = 3),
                  "%) caused errors.\n", sep= ""))
      }
    }
  }

  out$front.bw <- bw.opt
  
  # updating names of output
  list.names <- c('Param', 'bw', 'Half-bw', 'Double-bw')
  coeff.names <- c("ev1", "ev2", "ate", "htev1", "htev2", "htate", "tev1", "tev2", "tate")
  names(out$m_s) = names(out$m_h) = names(out$m_t) = names(out$dat_h) = names(out$obs) <- list.names
  rownames(out$est) = rownames(out$d) = rownames(out$se) <- list.names
  colnames(out$est) = colnames(out$d) = colnames(out$se) <- coeff.names
  if (is.numeric(boot) && boot > 0) {
    colnames(out$ci) <- coeff.names
    names(out$est_boot) <- list.names
    colnames(out$est_boot$'Param') = colnames(out$est_boot$'bw') = colnames(out$est_boot$'Half-bw') =
      colnames(out$est_boot$'Double-bw') <- coeff.names
  }
  
  return(out)
}


# k-fold cross validation
mfrd_est_cv <- function(y, x1, x2, c1, c2, t.design = NULL, local = 0.15, front.bw = 1, bw.test = 0.25,
                        k = 5, kernel = "triangular", ngrid = 250, margin = 0.03) {
  
  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  # Randomly shuffle the data
  set.seed(0)
  samp = sample(length(y))
  y <- y[samp]
  x1 <- x1[samp]
  x2 <- x2[samp]
  
  x1_tr <- treat_assign(x1, c1, t.design[1])
  x2_tr <- treat_assign(x2, c2, t.design[2])
  tr1 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & !x2_tr, 1, 0))
  tr2 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(!x1_tr & x2_tr, 1, 0))
  trb <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & x2_tr, 1, 0))
  tr <- ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
  
  # Create k equally size folds
  folds <- cut(seq(1, length(y)), breaks = k, labels = FALSE)
  
  mse = data.frame(mse_s = rep(NA, k), mse_h = rep(NA, k), mse_t = rep(NA, k))
  
  # Perform k-fold cross validation
  for(i in 1:k){
    
    #Segement your data by fold using the which() function 
    testInd <- which(folds == i, arr.ind = TRUE)
    testy <- y[testInd]
    testx1 <- x1[testInd]
    testx2 <- x2[testInd]
    trainy <- y[-testInd]
    trainx1 <- x1[-testInd]
    trainx2 <- x2[-testInd]
    
    # Fit using training data
    zc1 <- c(scale(c1, center = mean(trainx1), scale = sd(trainx1)))
    zc2 <- c(scale(c2, center = mean(trainx2), scale = sd(trainx2)))
    trainzx1 <- c(scale(trainx1))
    trainzx2 <- c(scale(trainx2))
    
    traindat <- data.frame(
      y.dat = trainy,
      tr1.dat = tr1[-testInd],
      tr2.dat = tr2[-testInd],
      trb.dat = trb[-testInd],
      tr.dat = tr[-testInd],
      
      zx1 = trainzx1,
      zx2 = trainzx2,
      
      zcx1 = trainzx1 - zc1,
      zcx2 = trainzx2 - zc2
    )
    
    trainwt = wt_kern_bivariate(traindat$zcx1, traindat$zcx2, 0, 0, front.bw, kernel = kernel, t.design = t.design)
    m_s <- lm(y.dat ~ zcx1 * zcx2 * (tr1.dat + tr2.dat) + tr.dat,  weights = trainwt$wAll1, data = traindat) 
    m_h <- lm(y.dat ~ zcx1 + zcx2 + tr.dat + tr1.dat + tr2.dat, weights = trainwt$wAll2, data = traindat) 
    m_t <- lm(y.dat ~ zcx1 + zcx2 + tr.dat, weights = trainwt$wTr, data = traindat) 
     
    # Return MSE of test data only within a fixed bandwidth bw.test
    testzx1 <- c(scale(testx1, sd(trainx1)))
    testzx2 <- c(scale(testx2, sd(trainx2)))
    
    testdat <- data.frame(
      y.dat = testy,
      tr1.dat = tr1[testInd],
      tr2.dat = tr2[testInd],
      trb.dat = trb[testInd],
      tr.dat = tr[testInd],
      
      zx1 = testzx1,
      zx2 = testzx2,
      
      zcx1 = testzx1 - zc1,
      zcx2 = testzx2 - zc2
    )

    testwt = wt_kern_bivariate(testdat$zcx1, testdat$zcx2, 0, 0, bw.test, kernel = kernel, t.design = t.design)
    yhat_s = predict.lm(m_s, newdata = testdat)
    yhat_h = predict.lm(m_h, newdata = testdat)
    yhat_t = predict.lm(m_t, newdata = testdat)
    
    mse$mse_s[i] = mean(((yhat_s - testy)[testwt$distAll1 <= 1])**2)
    mse$mse_h[i] = mean(((yhat_h - testy)[testwt$distAll2 <= 1])**2)
    mse$mse_t[i] = mean(((yhat_t - testy)[testwt$distTr <= 1])**2)
  }

  return(colMeans(mse))
}


## single estimate
mfrd_est_single <- function(y, x1, x2, c1, c2, 
                            t.design = c("l", "l"), local = 0.15, front.bw = NA, kernel = 'triangular',
                            ngrid = 250, margin = 0.03, stop.on.error = stop.on.error) {
  # call <- match.call()
  
  dat <- data.frame(y, x1, x2)
  
  zc1 <- c(scale(c1, center = mean(dat$x1), scale = sd(dat$x1)))
  zc2 <- c(scale(c2, center = mean(dat$x2), scale = sd(dat$x2)))
  
  if (!all(t.design %in% c("g", "geq", "l", "leq"))) {
    stop("Treatment design must be one of 'g', 'geq', 'l', 'leq'.")
  }
  
  x1_tr <- treat_assign(x1, c1, t.design[1])
  x2_tr <- treat_assign(x2, c2, t.design[2])
  
  tr1 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & !x2_tr, 1, 0))
  tr2 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(!x1_tr & x2_tr, 1, 0))
  trb <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & x2_tr, 1, 0))
  
  tr <- ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
  
  ### TRANSFORMAT VARIABLES ###
  
  dat <- within(dat, {
    tr1 = tr1
    tr2 = tr2
    trb = trb
    
    tr = ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
    
    zx1 = c(scale(x1))
    zx2 = c(scale(x2))
    
    zcx1 = zx1 - zc1
    zcx2 = zx2 - zc2
    
    # Sample restriction to obs "m" bw for math frontier
    x2res = as.integer(zcx2 <= local & zcx2 >= -local)
    
    # Sample restriction to obs "w" bw for reading frontier
    x1res = as.integer(zcx1 <= local & zcx1 >= -local)
  })
  
  # sharp rd only
  if(!is.null(tr))
    if(!all(dat$tr == as.integer(tr))){
      warning("Supplied treatment does not match derived treatment; 
              use derived treatment instead.")
      print(data.frame(supplied = as.integer(tr), derived = dat$tr))
    }
  
  ### FIT THE ORIGINAL DATASET ###
  
  if (all(is.na(front.bw))){
    ## Complete model ##
    m_s <- lm(y ~ zcx1 * zcx2 * (tr1 + tr2) + tr, data = dat) 
    
    ## Treatment indicators for only treatment planes ##
    m_h <- lm(y ~ zcx1 + zcx2 + tr + tr1 + tr2, data = dat) 
    
    ## Treatment indicator only ##
    m_t <- lm(y ~ zcx1 + zcx2 + tr, data = dat) 
    
    # models for output (can be used in predict() -- not using zcx1 and zcx2)
    m_s.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) * 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2))) - .(zc2)) * (tr1 + tr2) + tr)
    )) 
    
    m_h.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) + 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2)))- .(zc2)) + tr + tr1 + tr2, data = dat) 
    ))
    
    m_t.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) + 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2)))- .(zc2)) + tr, data = dat) 
    ))
    
    # number of observations used in each model
    obs = rep(length(y), 3)
  }else{
    wt <- wt_kern_bivariate(dat$zcx1, dat$zcx2, 0, 0, front.bw, kernel = kernel, t.design = t.design)
    
    ## Complete model ##
    m_s <- lm(y ~ zcx1 * zcx2 * (tr1 + tr2) + tr, weights = wt$wAll1, data = dat) 
    
    ## Treatment indicators for only treatment planes ##
    m_h <- lm(y ~ zcx1 + zcx2 + tr + tr1 + tr2, weights = wt$wAll2, data = dat) 
    
    ## Treatment indicator only ##
    m_t <- lm(y ~ zcx1 + zcx2 + tr, weights = wt$wTr, data = dat) 
    
    # models for output (can be used in predict() -- not using zcx1 and zcx2)
    m_s.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) * 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2))) - .(zc2)) * (tr1 + tr2) + tr,
         weights = wt$wAll1, data = dat)
    )) 
    
    m_h.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) + 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2)))- .(zc2)) + tr + tr1 + tr2,
         weights = wt$wAll2, data = dat) 
    ))
    
    m_t.o <- eval(bquote(
      lm(y ~ I(scale(x1, center = .(mean(x1)), scale = .(sd(x1))) - .(zc1)) + 
           I(scale(x2, center = .(mean(x2)), scale = .(sd(x2)))- .(zc2)) + tr, 
         weights = wt$wTr, data = dat) 
    ))   
    
    # number of observations used in each model
    obs = c(sum(wt$wAll1 > 0), sum(wt$wAll2 > 0), sum(wt$wTr > 0))
  }

  # test if output models are identical to the original ones
  # all.equal(m_s, m_s.o)  # TRUE (except for names)
  # all.equal(m_h, m_h.o)  # TRUE (except for names)
  # all.equal(m_t, m_t.o)  # TRUE (except for names)
  
  ### GENERATE A NEW DATASET ###
  
  N <- ngrid
  
  zcx1_min <- min(dat$zcx1) - margin * (max(dat$zcx1) - min(dat$zcx1))
  zcx1_max <- max(dat$zcx1) + margin * (max(dat$zcx1) - min(dat$zcx1))
  
  zcx2_min <- min(dat$zcx2) - margin * (max(dat$zcx2) - min(dat$zcx2))
  zcx2_max <- max(dat$zcx2) + margin * (max(dat$zcx2) - min(dat$zcx2))
  
  dat_h <- data.frame(
    h1 = c(rep(0, N), zcx1_min + 1:N / N * (zcx1_max - zcx1_min)),
    h2 = c(zcx2_min + 0:(N - 1) / N * (zcx2_max - zcx2_min), rep(0, N)),
    tr = 1,
    comp = 0,
    tr1 = rep(c(1, 0), each = N),
    tr2 = rep(c(0, 1), each = N)
  )
  
  #####################################################################
  # COMPLETE MODEL ####################################################
  #####################################################################
  
  # TREATMENT OUTCOME FUNCTIONS g(x)'s #############################
  # Function for treatment effect along MATH frontier
  dat_h$gx1 <- m_s$coefficients["tr"] * dat_h$tr + 
    m_s$coefficients["tr2"] * dat_h$tr2 + 
    m_s$coefficients["zcx1:tr2"] * dat_h$tr2 * (dat_h$h1 - zc1)
  
  # Function for treatment effect along READING frontier
  dat_h$gx2 <- m_s$coefficients["tr"] * dat_h$tr + 
    m_s$coefficients["tr1"] * dat_h$tr1 + 
    m_s$coefficients["zcx2:tr1"] * dat_h$tr1 * (dat_h$h2 - zc2)
  
  # DENSITY FUNCTIONS f(x)'s #######################################
  
  # Reading assignment variable / Math treatment frontier ##########
  
  # Kernel density of reading assignment variable
  # dat_h$fx1 = kdensity(dat$zx1[dat$x2res == 1], dat_h$h1)
  bw1 <- bw.nrd0(dat$zx1[dat$x2res == 1])
  dat_h$fx1 <- den_kern(x = dat$zx1[dat$x2res == 1], newx = dat_h$h1, bw = bw1, fun = tri_kern)
  
  # Product for numerator: g(x) * f(x)
  dat_h$x1prod <- ifelse(treat_assign(dat_h$h1, zc1, t.design[1]), NA, dat_h$gx1 * dat_h$fx1)
  
  # Numerator: Integral for numerator of MATH frontier (x1)
  dat_h_sub1 <- subset(dat_h, !as.logical(treat_assign(dat_h$h1, zc1, t.design[1])))
  numintx1 <- int_cubic(dat_h_sub1$h1, dat_h_sub1$x1prod, stop.on.error)
  
  # Denominator: Intergral of the conditional density function f1(x1) 
  denintx1 <- int_cubic(dat_h_sub1$h1, dat_h_sub1$fx1, stop.on.error)
  
  # Math assignment variable / Reading treatment frontier ##############
  
  # Kernel density of math assignment variable
  # dat_h$fx2 = kdensity(dat$zx2[dat$x1res == 1], dat_h$h2)
  bw2 <- bw.nrd0(dat$zx2[dat$x1res == 1])
  dat_h$fx2 <- den_kern(x = dat$zx2[dat$x1res == 1], newx = dat_h$h2, bw = bw2, fun = tri_kern)
  
  # Product for numerator: g(x) * f(x)
  dat_h$x2prod <- ifelse(treat_assign(dat_h$h2, zc2, t.design[2]), NA, dat_h$gx2 * dat_h$fx2)
  
  # Numerator: Integral for numerator of READING frontier (x2)
  dat_h_sub2 <- subset(dat_h, !as.logical(treat_assign(dat_h$h2, zc2, t.design[2])))
  numintx2 <- int_cubic(dat_h_sub2$h2, dat_h_sub2$x2prod, stop.on.error) 
  
  # Denominator: Intergral of the conditional density function f2(x2)
  denintx2 <- int_cubic(dat_h_sub2$h2, dat_h_sub2$fx2, stop.on.error) 
  
  # Treatment weights ################################################
  
  # Marginal density at math cutoff
  dat_h$ch2 <- abs(dat_h$h2 - zc2)
  md2cw <- mean(dat_h$fx2[dat_h$ch2 == min(dat_h$ch2)])
  
  # Marginal density at reading cutoff
  dat_h$ch1 <- abs(dat_h$h1 - zc1)
  md1cw <- mean(dat_h$fx1[dat_h$ch1 == min(dat_h$ch1)])
  
  # Weight for Math frontier
  w2 <- md2cw * denintx1
  
  # Weight for Reading frontier
  w1 <- md1cw * denintx2
  
  # Treatment estimates for complete model ##############################
  
  # Math frontier expected value
  ev2 <- numintx1 / denintx1
  
  # Reading frontier expected value
  ev1 <- numintx2 / denintx2 
  
  # ATE across both frontiers
  ate <- ((w2 * ev2) + (w1 * ev1)) / (w2 + w1)
  
  #####################################################################
  # HETEROGENEOUS TREATMENTS MODEL ####################################
  #####################################################################
  
  # Function for treatment effect along MATH frontier
  dat_h$htgx1 <- m_h$coefficients["tr"] * dat_h$tr + m_h$coefficients["tr2"] * dat_h$tr2
  
  # Function for treatment effect along READING frontier
  dat_h$htgx2 <- m_h$coefficients["tr"] * dat_h$tr + m_h$coefficients["tr1"] * dat_h$tr1
  
  # DENSITY FUNCTIONS f(x)'s #######################################
  
  # Reading assignment variable / Math treatment frontier ##########
  
  # Product for numerator: g(x) * f(x)
  dat_h$htx1prod <- ifelse(treat_assign(dat_h$h1, zc1, t.design[1]), NA, dat_h$htgx1 * dat_h$fx1)
  
  # Numerator: Integral for numerator of MATH frontier (x1)
  dat_h_sub1 <- subset(dat_h, !as.logical(treat_assign(dat_h$h1, zc1, t.design[1])))
  htnumintx1 <- int_cubic(dat_h_sub1$h1, dat_h_sub1$htx1prod, stop.on.error)
  
  # Math assignment variable / Reading treatment frontier ##############
  
  # Product for numerator: g(x) * f(x)
  dat_h$htx2prod <- ifelse(treat_assign(dat_h$h2, zc2, t.design[2]), NA, dat_h$htgx2 * dat_h$fx2)
  
  # Numerator: Integral for numerator of READING frontier (x2)
  dat_h_sub2 <- subset(dat_h, !as.logical(treat_assign(dat_h$h2, zc2, t.design[2])))
  htnumintx2 <- int_cubic(dat_h_sub2$h2, dat_h_sub2$htx2prod, stop.on.error) 
  
  # Treatment estimates for "heterogeneous treatments" (HT) model ######
  
  # Math frontier expected value
  htev2 <- htnumintx1 / denintx1 
  
  # Reading frontier expected value
  htev1 <- htnumintx2 / denintx2
  
  # ATE across both frontiers
  htate <- ((w2 * htev2) + (w1 * htev1)) / (w2 + w1)
  
  #####################################################################
  # TREATMENT ONLY MODEL ##############################################
  #####################################################################
  
  # Function for treatment effect along MATH frontier
  dat_h$tgx1 <- m_t$coefficients["tr"] * dat_h$tr 
  
  # Function for treatment effect along READING frontier
  dat_h$tgx2 <- m_t$coefficients["tr"] * dat_h$tr 
  
  # DENSITY FUNCTIONS f(x)'s #######################################
  
  # Reading assignment variable / Math treatment frontier ##########
  
  # Product for numerator: g(x) * f(x)
  dat_h$tx1prod <- ifelse(treat_assign(dat_h$h1, zc1, t.design[1]), NA, dat_h$tgx1 * dat_h$fx1)
  
  # Numerator: Integral for numerator of MATH frontier (x1)
  dat_h_sub1 <- subset(dat_h, !as.logical(treat_assign(dat_h$h1, zc1, t.design[1])))
  tnumintx1 <- int_cubic(dat_h_sub1$h1, dat_h_sub1$tx1prod, stop.on.error)
  
  # Math assignment variable / Reading treatment frontier ##############
  
  # Product for numerator: g(x) * f(x)
  dat_h$tx2prod <- ifelse(treat_assign(dat_h$h2, zc2, t.design[2]), NA, dat_h$tgx2 * dat_h$fx2)
  
  # Numerator: Integral for numerator of READING frontier (x2)
  dat_h_sub2 <- subset(dat_h, !as.logical(treat_assign(dat_h$h2, zc2, t.design[2])))
  tnumintx2 <- int_cubic(dat_h_sub2$h2, dat_h_sub2$tx2prod, stop.on.error)
  
  # Treatment estimates for "treatment only" (TO) model ######
  
  # Math frontier expected value
  tev2 <- tnumintx1 / denintx1 
  
  # Reading frontier expected value
  tev1 <- tnumintx2 / denintx2 
  
  # ATE across both frontiers
  tate <- ((w2 * tev2) + (w1 * tev1)) / (w2 + w1)
  
  out <- list()
  class(out) <- "mfrd"
  
  # out$call <- call
  
  out$w <- c(w1, w2)
  names(out$w) <- c("w1", "w2")
  
  out$est <- c(ev1, ev2, ate, htev1, htev2, htate, tev1, tev2, tate)
  
  names(out$est) <- c("ev1", "ev2", "ate", 
                      "htev1", "htev2", "htate",
                      "tev1", "tev2", "tate")
  out$d <- out$est / sd(y)
  out$se <- rep(NA, 9)
  
  # out$w1 <- w1        # weight of fronter 1
  # out$w2 <- w2        # weight of frontier 2
  # out$ev1 <- ev1      # frontier 1 for complete model 
  # out$ev2 <- ev2      # frontier 2 for complete model 
  # out$ate <- ate      # ATE for compelete model
  # out$htev1 <- htev1  # frontier 1 for heterogeneous treatment model 
  # out$htev2 <- htev2  # frontier 2 for heterogeneous treatment model 
  # out$htate <- htate  # ATE for heterogeneous treatment model
  # out$tev1 <- tev1    # frontier 1 for treatment only model 
  # out$tev2 <- tev2    # frontier 2 for treatment only model 
  # out$tate <- tate    # ATE for treatment only model
  
  out$m_s <- m_s.o
  out$m_h <- m_h.o 
  out$m_t <- m_t.o
  
  out$dat_h <- dat_h
  out$dat <- dat
  out$obs <- obs
  
  return(out)
}

## mimic Stata's integ
int_cubic <- function(x, y, stop.on.error = TRUE){
  integrate(splinefun(x, y, method = "natural"), lower = min(x), upper = max(x), 
            subdivisions = 500L, stop.on.error = stop.on.error)$value
}

## mimic Stata's kdensity
den_kern <- function(x, newx, bw, fun) {
  n <- length(x)
  newn <- length(newx)
  
  colMeans(fun((matrix(rep(newx, n), n, newn, byrow = TRUE) - x) / bw)) / bw
}

tri_kern <- function(x) {
  ifelse(abs(x) > 1, 0, 1 - abs(x))
}

# kdensity = function(x, at){
#   den = density(x, kernel = 'triangular')
#   approx(den$x, den$y, xout = at, rule = 2)$y  
# }

## cluster bootstrap
clus_boot <- function(n, cluster) {
  x <- 1:n
  clus_list <- unique(cluster)
  num_clus <- length(clus_list)
  
  for (i in 1:num_clus) {
    x[cluster == clus_list[i]] <- sample(x[cluster == clus_list[i]], replace = TRUE)
  }
  
  return(x)
}
