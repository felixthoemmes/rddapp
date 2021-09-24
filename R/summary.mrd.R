#' Summarize the Multivariate Regression Discontinuity
#' 
#' \code{summary.mrd} is a \code{summary} method for class \code{"mrd"}
#' It is based on summary.RD function in the rdd package. 
#' 
#' @method summary mrd
#' 
#' @param object An object of class \code{"mrd"}, usually a result of a call to \code{\link{mrd_est}}.
#' @param level Numerical value between 0 and 1. Confidence level for confidence intervals.
#' @param digits Number of digits to display.
#' @param ... Additional arguments.
#' 
#' @return \code{summary.mrd} returns a list which has the following components depending on methods
#' implemented in the \code{"mrd"} object:
#' \item{center_coefficients}{A matrix containing bandwidths, number of observations, estimates, 
#'   SEs, confidence intervals, z-values and p-values for each estimated bandwidth.}
#' \item{univR_coefficients}{A matrix containing bandwidths, number of observations, estimates, 
#'   SEs, confidence intervals, z-values and p-values for each estimated bandwidth.}
#' \item{univM_coefficients}{A matrix containing bandwidths, number of observations, estimates, 
#'   SEs, confidence intervals, z-values and p-values for each estimated bandwidth.}
#' \item{front_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the complete model.}
#' \item{front_ht_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the heterogeneous treatment model.}
#' \item{front_t_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the treatment only model.}
#' 
#' @importFrom stats residuals
#'
#' @include mrd_est.R
#' 
#' @export

summary.mrd <- function(object, level = 0.95, digits = max(3, getOption("digits") - 3), ...) {
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
  
  out = list()
  
  if ('center' %in% names(object)){
    cat("Centering Method:\n")
    center_obj = object$center$tau_MRD
    center_summary = summary(center_obj, level, digits)
    center_out = center_summary$coefficients
    out[['center_coefficients']] = center_out
    cat("\n")
  }
  
  if ('univ' %in% names(object)){
    cat("Univariate Method on Frontier R:\n")
    univR_obj = object$univ$tau_R
    univR_summary = summary(univR_obj, level, digits)
    univR_out = univR_summary$coefficients
    out[['univR_coefficients']] = univR_out
    
    cat("Univariate Method on Frontier M:\n")
    univM_obj = object$univ$tau_M
    univM_summary = summary(univM_obj, level, digits)
    univM_out = univM_summary$coefficients
    out[['univM_coefficients']] = univM_out
    cat("\n")
  }

  if ('front' %in% names(object)){
    cat("Frontier Method:\n")
    front_obj = object$front$tau_MRD
    front_summary = summary(front_obj, level, digits)
    front_out = front_summary$coefficients
    front_ht_out = front_summary$ht_coefficients
    front_t_out = front_summary$t_coefficients
    out[['front_coefficients']] = front_out
    out[['front_ht_coefficients']] = front_ht_out
    out[['front_t_coefficients']] = front_t_out
    cat("\n")
  }

  return(invisible(out))
} 