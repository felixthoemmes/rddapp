#' Imbens-Kalyanaraman 2012 Optimal Bandwidth Calculation
#' 
#' \code{bw_ik12} calculates the Imbens-Kalyanaraman (2012) optimal bandwidth
#' for local linear regression in regression discontinuity designs.
#' It is based on a function in the "rddtools" package. 
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::bw_ik12().
#' 
#' @param X A numeric vector containing the running variable.
#' @param Y A numeric vector containing the outcome variable.
#' @param cutpoint A numeric vector of length 1 containing the cutpoint at which assignment to the treatment is determined.
#' @param verbose A logical value indicating whether to print more information to the terminal.
#'   The default is \code{FALSE}.
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}. 
#'
#' @return `ik_bw12` returns a numeric vector of length 1 containing the optimal bandwidth.
#' 
#' @references Imbens, G., Kalyanaraman, K. (2012). 
#'   Optimal bandwidth choice for the regression discontinuity estimator. 
#'   The Review of Economic Studies, 79(3), 933-959.
#'   \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
#'   
#'   Stigler, M. and B. Quast, B (2016). rddtools: A toolbox for regression discontinuity in R.
#'   
#'
#' @importFrom stats var

bw_ik12 <- function(X, Y, cutpoint = NULL, verbose = FALSE, kernel = "triangular") {
  # type <- match.arg(type)
  # kernel <- match.arg(kernel)
  
  sub <- complete.cases(X) & complete.cases(Y)
  X <- X[sub]
  Y <- Y[sub]
  
  N <- length(X)
  N_left <- sum(X < cutpoint, na.rm = TRUE)
  N_right <- sum(X >= cutpoint, na.rm = TRUE)
  
  if (N != length(Y)) 
    stop("Running and outcome variable must be of equal length.")
  
  if (is.null(cutpoint)) {
    cutpoint <- 0
    if (verbose) 
      cat("Using default cutpoint of zero.\n")
  } else {
    if (!(typeof(cutpoint) %in% c("integer", "double"))) 
      stop("Cutpoint must be of a numeric type.")
  }
  
  ########## STEP 1
  
  ## Silverman bandwidth
  h1 <- 1.84 * sd(X) * N^(-1/5)
  if (verbose) 
    cat("\n-h1:", h1)
  
  ## f(cut)
  isIn_h1_left <- X >= (cutpoint - h1) & X < cutpoint
  isIn_h1_right <- X >= cutpoint & X <= (cutpoint + h1)
  
  NisIn_h1_left <- sum(isIn_h1_left, na.rm = TRUE)
  NisIn_h1_right <- sum(isIn_h1_right, na.rm = TRUE)
  if (verbose) 
    cat("\n-N left/right:", NisIn_h1_left, NisIn_h1_right)
  
  f_cut <- (NisIn_h1_left + NisIn_h1_right) / (2 * N * h1)
  if (verbose) 
    cat("\n-f(cutpoint):", f_cut)
  
  ## Variances : Equ (13)
  
  var_inh_left <- var(Y[isIn_h1_left], na.rm = TRUE)
  var_inh_right <- var(Y[isIn_h1_right], na.rm = TRUE)

  if (verbose) {
    cat("\n-Sigma^2 left:", var_inh_left, "\n-Sigma^2 right:", var_inh_right)
  }
  
  ########## STEP 2
  
  ## Global function of order 3: Equ (14)
  reg <- lm(Y ~ I(X >= cutpoint) + I(X - cutpoint) + I((X - cutpoint)^2) + I((X - cutpoint)^3))
  m3 <- 6 * coef(reg)[5]
  if (verbose) 
    cat("\n-m3:", m3)
  
  ## left and right bandwidths: Equ (15)
  Ck_h2 <- 3.5567  # 7200^(1/7)
  h2_left <- Ck_h2 * (var_inh_left / (f_cut * m3^2))^(1/7) * N_left^(-1/7)
  h2_right <- Ck_h2 * (var_inh_right / (f_cut * m3^2))^(1/7) * N_right^(-1/7)
  
  if (verbose) 
    cat("\n-h2 left:", h2_left, "\n-h2 right:", h2_right)
  
  ## second derivatives right/left
  isIn_h2_left <- X >= (cutpoint - h2_left) & X < cutpoint
  isIn_h2_right <- X >= cutpoint & X <= (cutpoint + h2_right)
  
  N_h2_left <- sum(isIn_h2_left, na.rm = TRUE)
  N_h2_right <- sum(isIn_h2_right, na.rm = TRUE)
  
  if (N_h2_left == 0 | N_h2_right == 0) 
    stop("Insufficient data in vicinity of the cutpoint to calculate bandwidth.")
  
  reg2_left <- lm(Y ~ I(X - cutpoint) + I((X - cutpoint)^2), subset = isIn_h2_left)
  reg2_right <- lm(Y ~ I(X - cutpoint) + I((X - cutpoint)^2), subset = isIn_h2_right)
  
  m2_left <- as.numeric(2 * coef(reg2_left)[3])
  m2_right <- as.numeric(2 * coef(reg2_right)[3])
  
  if (verbose) 
    cat("\n-m2 left:", m2_left, "\n-m2 right:", m2_right)
  
  ########## STEP 3
  
  ## Regularization: Equ (16)
  r_left <- (2160 * var_inh_left) / (N_h2_left * h2_left^4)
  r_right <- (2160 * var_inh_right) / (N_h2_right * h2_right^4)
  
  if (verbose) 
    cat("\n-Reg left:", r_left, "\n-Reg right:", r_right)
  
  # Which kernel are we using?  
  # Method for finding these available in I--K p. 6
  if (kernel == "triangular") {
    ck <- 3.43754
  } else if (kernel == "rectangular") {
    ck <- 2.70192
  } else if (kernel == "epanechnikov") {
    ck <- 3.1999
  } else if (kernel == "quartic" | kernel == "biweight") {
    ck <- 3.65362
  } else if (kernel == "triweight") {
    ck <- 4.06065
  } else if (kernel == "tricube") {
    ck <- 3.68765
  # } else if (kernel == "gaussian") {
  #   ck <- 1.25864
  } else if (kernel == "cosine") {
    ck <- 3.25869
  } else {
    stop("Unrecognized kernel.")
  }
  
  ## Final bandwidth: Equ (17)
  optbw <- ck * ((var_inh_left + var_inh_right) / 
    (f_cut * ((m2_right - m2_left)^2 + r_left + r_right)))^(1/5) * N^(-1/5)
  
  left <- (X >= (cutpoint - optbw)) & (X < cutpoint)
  right <- (X >= cutpoint) & (X <= (cutpoint + optbw))
  if (sum(left) == 0 | sum(right) == 0) 
    stop("Insufficient data in the calculated bandwidth.")
  names(optbw) <- NULL
  if (verbose) 
    cat("Imbens-Kalyanamaran Optimal Bandwidth: ", sprintf("%.3f", optbw), "\n")
  return(optbw)
} 

