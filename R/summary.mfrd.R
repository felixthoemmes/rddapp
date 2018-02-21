#' Summarize the Multivariate Frontier Regression Discontinuity
#' 
#' \code{summary.mfrd} is a \code{summary} method for class \code{"mfrd"}
#' It is based on \code{\link[rdd]{summary.RD}} function in the \pkg{rdd} package. 
#' 
#' @method summary mfrd
#' 
#' @param object An object of class \code{"mfrd"}, usually a result of a call to \code{\link{mfrd_est}}.
#' @param digits Number of digits to display.
#' @param ... Additional arguments.
#' 
#' @return \code{summary.mfrd} returns a list which has the following components:
#' \item{coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the complete model.}
#' \item{ht_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the heterogeneous treatment model.}
#' \item{t_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the treatment only model.}
#' 
#' @importFrom stats residuals
#'
#' @include mfrd_est.R
#' 
#' @export

summary.mfrd <- function(object, digits = max(3, getOption("digits") - 3), ...) {
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
  
  cat("Estimates for Complete Model:\n")
  
  if ('ci' %in% names(object)){
    out <- matrix(c(object$est[1:3], t(object$ci[,1:3])), ncol = 3, byrow = FALSE)
    rownames(out) <- names(object$est[1:3])
    colnames(out) <- c('Estimate', 'Lower Bound of 95% CI', 'Upper Bound of 95% CI')
  }else{
    out <- matrix(object$est[1:3], ncol = 1)
    rownames(out) <- names(object$est[1:3])
    colnames(out) <- 'Estimate'
  }
  
  print.default(apply(out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  
  cat("Estimates for Heterogeneous Treatment Model:\n")
  
  if ('ci' %in% names(object)){
    ht_out <- matrix(c(object$est[4:6], t(object$ci[,4:6])), ncol = 3, byrow = FALSE)
    rownames(ht_out) <- names(object$est[4:6])
    colnames(ht_out) <- c('Estimate', 'Lower Bound of 95% CI', 'Upper Bound of 95% CI')
  }else{
    ht_out <- matrix(object$est[4:6], ncol = 1)
    rownames(ht_out) <- names(object$est[4:6])
    colnames(ht_out) <- 'Estimate'
  }
  
  print.default(apply(ht_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  
  cat("Estimates for Treatment Only Model:\n")
  
  if ('ci' %in% names(object)){
    t_out <- matrix(c(object$est[7:9], t(object$ci[,7:9])), ncol = 3, byrow = FALSE)
    rownames(t_out) <- names(object$est[7:9])
    colnames(t_out) <- c('Estimate', 'Lower Bound of 95% CI', 'Upper Bound of 95% CI')
  }else{
    t_out <- matrix(object$est[7:9], ncol = 1)
    rownames(t_out) <- names(object$est[7:9])
    colnames(t_out) <- 'Estimate'
  }
  
  print.default(apply(t_out, 2, function(x) format(x, digits = digits)), 
                quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  
  all_out <- list(coefficients = out, ht_coefficients = ht_out, t_coefficients = t_out)
  
  return(invisible(all_out))
} 