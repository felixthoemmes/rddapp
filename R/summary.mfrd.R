#' Summarize the Multivariate Frontier Regression Discontinuity
#' 
#' \code{summary.mfrd} is a \code{summary} method for class \code{"mfrd"}.
#' It is based on the \code{summary.RD} function in the "rdd" package. 
#' 
#' @method summary mfrd
#' 
#' @param object An object of class \code{"mfrd"}, usually a result of a call to \code{\link{mfrd_est}}.
#' @param level A numeric value between 0 and 1 specifying the confidence level for confidence intervals. The default is 0.95.
#' @param digits A non-negative integer specifying the number of digits to display.
#' @param ... Additional arguments passed to \code{summary}.
#' 
#' @return \code{summary.mfrd} returns a list containing the following components:
#' \item{coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the complete model.}
#' \item{ht_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the heterogeneous treatment model.}
#' \item{t_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the treatment only model.}
#' 
#' @references Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
#'    version 0.57. https://CRAN.R-project.org/package=rdd
#' 
#' @importFrom stats residuals
#'
#' @include mfrd_est.R
#' 
#' @export

summary.mfrd <- function(object, level = 0.95, digits = max(3, getOption("digits") - 3), ...) {
  call.copy <- object$call
  if ("data" %in% names(call.copy) && length(call.copy$data) > 1) {
    call.copy$data <- "(.)"
  }
  if ("subset" %in% names(call.copy) && length(call.copy$subset) > 1) {
    call.copy$subset <- "(.)"
  }
  
  cat("\n")
  cat("Call:\n")
  # print(object$call)
  print(call.copy)
  cat("\n")
  
  cat("Weight of Frontier 1 and 2:\n")
  cat("w1:", format(object$w[1], digits = digits), "\n")
  cat("w2:", format(object$w[2], digits = digits), "\n\n")
  
  if ('ci' %in% names(object)){
    alpha <- 1 - level
    param.ci_boot <- apply(object$est_boot$'Param', 2, quantile, na.rm = TRUE, probs = c(alpha/2, 1-alpha/2))
    bw.ci_boot <- apply(object$est_boot$'bw', 2, quantile, na.rm = TRUE, probs = c(alpha/2, 1-alpha/2))
    half.ci_boot <- apply(object$est_boot$'Half-bw', 2, quantile, na.rm = TRUE, probs = c(alpha/2, 1-alpha/2))
    double.ci_boot <- apply(object$est_boot$'Double-bw', 2, quantile, na.rm = TRUE, probs = c(alpha/2, 1-alpha/2))
  }
  
  cat("Estimates for Complete Model:\n")

  if ('ci' %in% names(object)){
    param.out <- matrix(c(object$est['Param', 1:3], t(param.ci_boot[,1:3])), ncol = 3, byrow = FALSE)
    bw.out <- matrix(c(object$est['bw', 1:3], t(bw.ci_boot[,1:3])), ncol = 3, byrow = FALSE)
    half.out <- matrix(c(object$est['Half-bw', 1:3], t(half.ci_boot[,1:3])), ncol = 3, byrow = FALSE)
    double.out <- matrix(c(object$est['Double-bw', 1:3], t(double.ci_boot[,1:3])), ncol = 3, byrow = FALSE)
    rownames(param.out) = rownames(bw.out) = rownames(half.out) = rownames(double.out) <- colnames(object$est)[1:3]
    colnames(param.out) = colnames(bw.out) = colnames(half.out) = colnames(double.out) <- c('Estimate', 'lower.CL', 'upper.CL')
  }else{
    param.out <- matrix(object$est['Param', 1:3], ncol = 1)
    bw.out <- matrix(object$est['bw', 1:3], ncol = 1)
    half.out <- matrix(object$est['Half-bw', 1:3], ncol = 1)
    double.out <- matrix(object$est['Double-bw', 1:3], ncol = 1)
    rownames(param.out) = rownames(bw.out) = rownames(half.out) = rownames(double.out) <- colnames(object$est)[1:3]
    colnames(param.out) = colnames(bw.out) = colnames(half.out) = colnames(double.out) <- 'Estimate'
  }
  
  cat("Parametric:\n")
  print.default(apply(param.out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with bandwidth ", format(object$front.bw[1], digits = digits), ":\n",
            sep = ''))
  print.default(apply(bw.out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with half bandwidth:\n",
            sep = ''))
  print.default(apply(half.out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with double bandwidth:\n",
            sep = ''))
  print.default(apply(double.out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  
  cat("Estimates for Heterogeneous Treatment Model:\n")
  
  if ('ci' %in% names(object)){
    param.ht_out <- matrix(c(object$est['Param', 4:6], t(param.ci_boot[,4:6])), ncol = 3, byrow = FALSE)
    bw.ht_out <- matrix(c(object$est['bw', 4:6], t(bw.ci_boot[,4:6])), ncol = 3, byrow = FALSE)
    half.ht_out <- matrix(c(object$est['Half-bw', 4:6], t(half.ci_boot[,4:6])), ncol = 3, byrow = FALSE)
    double.ht_out <- matrix(c(object$est['Double-bw', 4:6], t(double.ci_boot[,4:6])), ncol = 3, byrow = FALSE)
    rownames(param.ht_out) = rownames(bw.ht_out) = rownames(half.ht_out) = rownames(double.ht_out) <- colnames(object$est)[4:6]
    colnames(param.ht_out) = colnames(bw.ht_out) = colnames(half.ht_out) = colnames(double.ht_out) <- c('Estimate', 'lower.CL', 'upper.CL')
  }else{
    param.ht_out <- matrix(object$est['Param', 4:6], ncol = 1)
    bw.ht_out <- matrix(object$est['bw', 4:6], ncol = 1)
    half.ht_out <- matrix(object$est['Half-bw', 4:6], ncol = 1)
    double.ht_out <- matrix(object$est['Double-bw', 4:6], ncol = 1)
    rownames(param.ht_out) = rownames(bw.ht_out) = rownames(half.ht_out) = rownames(double.ht_out) <- colnames(object$est)[4:6]
    colnames(param.ht_out) = colnames(bw.ht_out) = colnames(half.ht_out) = colnames(double.ht_out) <- 'Estimate'
  }
  
  cat("Parametric:\n")
  print.default(apply(param.ht_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with bandwidth ", format(object$front.bw[2], digits = digits), ":\n",
            sep = ''))
  print.default(apply(bw.ht_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with half bandwidth:\n",
            sep = ''))
  print.default(apply(half.ht_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with double bandwidth:\n",
            sep = ''))
  print.default(apply(double.ht_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)  
  cat("\n")
  
  cat("Estimates for Treatment Only Model:\n")
  
  if ('ci' %in% names(object)){
    param.t_out <- matrix(c(object$est['Param', 7:9], t(param.ci_boot[,7:9])), ncol = 3, byrow = FALSE)
    bw.t_out <- matrix(c(object$est['bw', 7:9], t(bw.ci_boot[,7:9])), ncol = 3, byrow = FALSE)
    half.t_out <- matrix(c(object$est['Half-bw', 7:9], t(half.ci_boot[,7:9])), ncol = 3, byrow = FALSE)
    double.t_out <- matrix(c(object$est['Double-bw', 7:9], t(double.ci_boot[,7:9])), ncol = 3, byrow = FALSE)
    rownames(param.t_out) = rownames(bw.t_out) = rownames(half.t_out) = rownames(double.t_out) <- colnames(object$est)[7:9]
    colnames(param.t_out) = colnames(bw.t_out) = colnames(half.t_out) = colnames(double.t_out) <- c('Estimate', 'lower.CL', 'upper.CL')
  }else{
    param.t_out <- matrix(object$est['Param', 7:9], ncol = 1)
    bw.t_out <- matrix(object$est['bw', 7:9], ncol = 1)
    half.t_out <- matrix(object$est['Half-bw', 7:9], ncol = 1)
    double.t_out <- matrix(object$est['Double-bw', 7:9], ncol = 1)
    rownames(param.t_out) = rownames(bw.t_out) = rownames(half.t_out) = rownames(double.t_out) <- colnames(object$est)[7:9]
    colnames(param.t_out) = colnames(bw.t_out) = colnames(half.t_out) = colnames(double.t_out) <- 'Estimate'
  }
  
  cat("Parametric:\n")
  print.default(apply(param.t_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with bandwidth ", format(object$front.bw[3], digits = digits), ":\n",
            sep = ''))
  print.default(apply(bw.t_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with half bandwidth:\n",
            sep = ''))
  print.default(apply(half.t_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat(paste("Non-parametric with double bandwidth:\n",
            sep = ''))
  print.default(apply(double.t_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)   
  cat("\n")
  if ('ci' %in% names(object)){
    cat("Confidence interval used: ", level, "\n\n")
  }
  
  out = list('Param' = param.out, 'bw' = bw.out, 'Half-bw' = half.out, 'Double-bw' = double.out)
  ht_out = list('Param' = param.ht_out, 'bw' = bw.ht_out, 'Half-bw' = half.ht_out, 'Double-bw' = double.ht_out)
  t_out = list('Param' = param.out, 'bw' = bw.t_out, 'Half-bw' = half.t_out, 'Double-bw' = double.t_out)
  all_out <- list(coefficients = out, ht_coefficients = ht_out, t_coefficients = t_out)
  
  return(invisible(all_out))
} 