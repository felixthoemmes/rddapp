#' Summarize the Multiple Imputation of Multivariate Regression Discontinuity Estimation
#' 
#' \code{summary.mrdi} is a \code{summary} method for class \code{"mrdi"}
#' 
#' @method summary mrdi
#' 
#' @param object An object of class \code{"mrdi"}, usually a result of a call to 
#' \code{\link{mrd_impute}} with \code{"front"} method.
#' @param level Numerical value between 0 and 1. Confidence level for confidence intervals.
#' @param digits Number of digits to display.
#' @param ... Additional arguments.
#' 
#' @return \code{summary.mrdi} returns a list which has the following components:
#' \item{coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the complete model.}
#' \item{ht_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the heterogeneous treatment model.}
#' \item{t_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the treatment only model.}
#' 
#' @importFrom stats residuals
#'
#' @include mrd_impute.R
#' 
#' @export

summary.mrdi <- function(object, level = 0.95, digits = max(3, getOption("digits") - 3), ...) {
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
  
  obj.front = object$front$tau_MRD
  n = length(obj.front$est)
  if (!all(is.na(obj.front$z))){
    alpha <- 1 - level
    lower.CL <- obj.front$est - qt(1-alpha/2, df = obj.front$df) * obj.front$se
    upper.CL <- obj.front$est + qt(1-alpha/2, df = obj.front$df) * obj.front$se
    
    stars <- vector(length = n)
    
    for (i in 1:n) {
      stars[i] <- if (is.na(obj.front$p[i])) 
        " " else if (obj.front$p[i] < 0.001) 
          "***" else if (obj.front$p[i] < 0.01) 
            "**" else if (obj.front$p[i] < 0.05) 
              "*" else if (obj.front$p[i] < 0.1) 
                "." else " "
    }
  }
  
  if (!all(is.na(obj.front$z))){
    out.all <- cbind(obj.front$est, obj.front$se, lower.CL, upper.CL, obj.front$z, obj.front$df, 
                     obj.front$p)
    colnames(out.all) <- c("Estimate", "Std. Error", "lower.CL", "upper.CL", 
                           "t value", "df", "Pr(>|t|)")
    outmat <- cbind(apply(out.all, 2, function(x) format(x, digits = digits)), " " = stars)
  } else {
    out.all <- matrix(obj.front$est, ncol = 1)
    rownames(out.all) <- names(obj.front$est)
    colnames(out.all) <- c("Estimate")
    outmat <- apply(out.all, 2, function(x) format(x, digits = digits))
  }
  
  cat("Estimates for Complete Model:\n")
  
  if (!all(is.na(obj.front$z))){
    out <- out.all[1:3,]
    print.default(outmat[1:3,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    out <- matrix(out.all[1:3,])
    colnames(out) <- colnames(out.all)
    rownames(out) <- rownames(out.all)[1:3]
    print.default(matrix(outmat[1:3,], dimnames = list(rownames(out), colnames(out))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  cat("Estimates for Heterogeneous Treatment Model:\n")
  
  if (!all(is.na(obj.front$z))){
    ht_out <- out.all[4:6,]
    print.default(outmat[4:6,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    ht_out <- matrix(out.all[4:6,])
    colnames(ht_out) <- colnames(out.all)
    rownames(ht_out) <- rownames(out.all)[4:6]
    print.default(matrix(outmat[4:6,], dimnames = list(rownames(out), colnames(out))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  cat("Estimates for Treatment Only Model:\n")
  
  if (!all(is.na(obj.front$z))){
    t_out <- out.all[7:9,]
    print.default(outmat[7:9,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    t_out <- matrix(out.all[7:9,])
    colnames(t_out) <- colnames(out.all)
    rownames(t_out) <- rownames(out.all)[7:9]
    print.default(matrix(outmat[7:9,], dimnames = list(rownames(out), colnames(out))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  all_out <- list(coefficients = out, ht_coefficients = ht_out, t_coefficients = t_out)
  
  return(invisible(all_out))
} 