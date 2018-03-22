#' Multivariate Frontier Regression Discontinuity Estimation 
#'
#' \code{mfrd_est} implements the frontier approach in Wong, Steiner and Cook (2013). 
#' It is based on the MFRDD code in Stata. 
#'
#' @param y The outcome variable (continuous).
#' @param x1 The assignment variable 1.
#' @param x2 The assignment variable 2.
#' @param c1 The cutoff of assignment variable 1.
#' @param c2 The cutoff of assignment variable 2.
#' # @param tr The treatment variable used to compare with the derived treatment 
#' based on assignments and cutoffs.
#' @param t.design The treatment option according to design.
#'   The 1st entry is for x1: \code{"g"} means treatment is assigned 
#'   if x1 is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if x1 is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if x1 is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if x1 is less than or equal to its cutoff.
#'   The 2nd entry is for x2.
#' @param local The range of neighboring points around the cutoff on the 
#'   standardized The scale on each assignment variable, which is a positive number.
#' @param ngrid The number of non-zero grid points on each assignment variable,
#'   which is also the number of zero grid points on each assignment variable.
#' @param margin The range of grid points beyond the minimum and maximum
#'   of sample points on each assignment variable.
#' @param boot The number of bootstrap samples to obtain standard error of estimates.
#' @param cluster An optional vector specifying clusters within which the errors are assumed
#'   to be correlated. This will result in reporting cluster robust SEs. This option overrides
#'   anything specified in \code{se.type}. It is suggested that data with a discrete running 
#'   variable be clustered by each unique value of the running variable (Lee and Card, 2008).
#' @param stop.on.error Logical. If \code{TRUE} (the default), removes bootstraps which cause
#'   error in the \code{integrate} function, and resample till the specified number of 
#'   bootstrap samples are acquired.
#'
#' @return \code{mfrd_est} returns an object of \link{class} "\code{mfrd}".
#'
#' @importFrom stats bw.nrd0 integrate splinefun
#'
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
#' mfrd_est(y = y, x1 = x1, x2 = x2, c1 = 0, c2 = 0, t.design = c("geq", "geq"))

mfrd_est <- function(y, x1, x2, c1, c2, t.design = NULL,
  local = 0.15, ngrid = 2500, margin = 0.03, boot = NULL, cluster = NULL,
  stop.on.error = TRUE) {
  
  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  call <- match.call()

  out <- mfrd_est_single(y = y, x1 = x1, x2 = x2, c1 = c1, c2 = c2, 
    t.design = t.design, local = local, ngrid = ngrid, margin = margin)
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
      tryboot <- try(mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], 
        c1 = c1, c2 = c2, t.design = t.design, local = local, 
        ngrid = ngrid, margin = margin, stop.on.error = TRUE)$est, silent = TRUE)
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
          est_boot[b, ] <- mfrd_est_single(y = y[index], x1 = x1[index], x2 = x2[index], 
                                           c1 = c1, c2 = c2, t.design = t.design, 
                                           local = local, ngrid = ngrid, margin = margin, 
                                           stop.on.error = stop.on.error)$est
        }else{
          est_boot[b, ] <- tryboot 
        }
        b <- b + 1
      }

    }
    out$est_boot = est_boot
    se_boot <- apply(est_boot, 2, sd, na.rm = TRUE)
    ci_boot <- apply(est_boot, 2, quantile, na.rm = TRUE, probs = c(.025,.975))
    out$se <- se_boot
    out$ci <- ci_boot
  }

  out$impute <- FALSE
  out$call <- call

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
  
  return(out)
}

## single estimate
mfrd_est_single <- function(y, x1, x2, c1, c2, t.design = c("l", "l"), 
  local = 0.15, ngrid = 2500, margin = 0.03, stop.on.error = stop.on.error) {
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