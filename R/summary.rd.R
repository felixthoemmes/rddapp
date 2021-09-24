#' Summarize the Regression Discontinuity
#' 
#' \code{summary.rd} is a \code{summary} method for class \code{"rd"}
#' It is based on summary.RD function in the rdd package. 
#' 
#' @method summary rd
#' 
#' @param object An object of class \code{"rd"}, usually a result of a call to \code{\link{rd_est}}.
#' @param level Numerical value between 0 and 1. Confidence level for confidence intervals.
#' @param digits Number of digits to display.
#' @param ... Additional arguments.
#' 
#' @return \code{summary.rd} returns a list which has the following components:
#' \item{coefficients}{A matrix containing bandwidths, number of observations, estimates, 
#'   SEs, confidence intervals, z-values and p-values for each estimated bandwidth.}
#' 
#' @importFrom stats residuals
#'
#' @include rd_est.R
#' 
#' @export

summary.rd <- function(object, level = 0.95, digits = max(3, getOption("digits") - 3), ...) {
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
  
  if (is.null(object$call$est.itt)){
    est.itt = 'FALSE'
  }else{
    if (object$call$est.itt == 'T' || object$call$est.itt == 'TRUE'){
      est.itt = 'TRUE'
    }else{
      est.itt = 'FALSE'
    }
  }
  cat("ITT used:\n")
  cat(est.itt)
  cat("\n\n")
  
  cat("Type:\n")
  cat(object$type, "\n\n")
  
  # If the model wasn't included in the output, we need to get it
  # mod <- FALSE
  # if ("model" %in% names(object$call)) 
  #   mod <- object$call$model
  # if (mod == 'F' || !mod) {
  #   object$call$model <- TRUE
  #   object$call$verbose <- FALSE
  #   object <- eval.parent(object$call)
  # }
  
  # if ("est.cov" %in% names(object$call) && eval(object$call$est.cov)) {
  #   k <- 1 + length(object$call$cov)
  # } else {
  #   k <- 1
  # }

  m <- length(object$est)
  n <- length(object$obs) 
  k <- m / n

  obs <- vector(length = n)
  if (object$type == "sharp") {
    for (i in 1:n) obs[i] <- length(residuals(object$model[[i]]))
  } else {
    for (i in 1:n) obs[i] <- length(residuals(object$model$iv[[i]]))
  }

  # Need to get this to give at least as much as stata does in fuzzy designs
  stars <- vector(length = n)
  
  for (i in 1:n) {
    stars[i] <- if (is.na(object$p[i])) 
     " " else if (object$p[i] < 0.001) 
      "***" else if (object$p[i] < 0.01) 
        "**" else if (object$p[i] < 0.05) 
          "*" else if (object$p[i] < 0.1) 
            "." else " "
  }
  
  # out <- matrix(c(rep(object$bw, each = k), rep(object$obs, each = k), 
  #                 object$est, object$se, object$z, object$p), nrow = m)
  # rownames(out) <- names(object$est)

  alpha <- 1 - level
  lower.CL <- object$est - qnorm(1-alpha/2) * object$se
  upper.CL <- object$est + qnorm(1-alpha/2) * object$se
  if (object$impute) {
    out <- matrix(c(rep(object$bw, each = k), rep(object$obs, each = k), 
      object$est, object$se, lower.CL, upper.CL, object$z, object$df, object$p), nrow = m)
    colnames(out) <- c("Bandwidth", "Observations", "Estimate", "Std. Error", 
                       "lower.CL", "upper.CL",
                       "t value", "df", "Pr(>|t|)")
  } else {
    out <- matrix(c(rep(object$bw, each = k), rep(object$obs, each = k), 
      object$est, object$se, lower.CL, upper.CL, object$z, object$p), nrow = m)
    colnames(out) <- c("Bandwidth", "Observations", "Estimate", "Std. Error", 
                       "lower.CL", "upper.CL",
                       "z value", "Pr(>|z|)") 
  }
  
  rownames(out) <- names(object$est)

  # if there are estimates of covariates, display in a separate table
  splitcov = FALSE
  if (!is.null(object$cov) && !is.null(object$call$est.cov)){
    if (object$call$est.cov == 'T' || object$call$est.cov == 'TRUE'){
      splitcov = TRUE
    }
  }
  
  outmat = cbind(apply(out, 2, function(x) format(x, digits = digits)), " " = stars)
  if (splitcov){
    outmat.est = outmat[seq(1, nrow(outmat), by = 2),]
    outmat.cov = outmat[seq(2, nrow(outmat), by = 2),]
    out = list(out[seq(1, nrow(out), by = 2),], out[seq(2, nrow(out), by = 2),])
    
    cat("Estimates:\n")
    print.default(outmat.est, quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
    
    cat("Estimates of covariates:\n")
    print.default(outmat.cov, quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
        
  }else{
    cat("Estimates:\n")
    print.default(outmat, quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }
  
  cat("Confidence interval used: ", level, "\n\n")
  
  out <- list(coefficients = out)
  # class(out) <- "summary.rd"
  
  # fstat <- matrix(NA, nrow = n, ncol = 4)
  # if (object$type == "sharp") {
  #   for (i in 1:n) {
  #     fstat[i, ] <- tryCatch({
  #       summ <- summary(object$model[[i]])
  #       c(summ$fstatistic[[1]], 
  #         summ$fstatistic[[2]], 
  #         summ$fstatistic[[3]], 
  #         pf(summ$fstatistic[[1]], 
  #           summ$fstatistic[[2]], 
  #           summ$fstatistic[[3]]))
  #     },
  #       error = function(e) {
  #         rep(NA, 4)
  #       }
  #     )
  #   }

  #   # use two-sided f test to calculate the p-value, 
  #   # which can be used for testing equality of variance
  #   fstat[, 4] <- 2 * apply(cbind(fstat[, 4], 1 - fstat[, 4]), 1, min)
  # } else {
  #   for (i in 1:n) {
  #     fstat[i, ] <- tryCatch({
  #       summ <- summary(object$model$iv[[i]])
  #       c(summ$waldtest[1], 
  #         summ$waldtest[3], 
  #         summ$waldtest[4], 
  #         summ$waldtest[2])
  #     },
  #       error = function(e) {
  #         rep(NA, 4)
  #       }
  #     )
  #   }
  # }

  # colnames(fstat) <- c("F", "Num. DoF", "Denom. DoF", "p")
  # rownames(fstat) <- names(object$bw)
  # cat("F-statistics:\n")
  # print.default(apply(fstat, 2, function(x) format(x, digits = digits)), quote = FALSE, 
  #   print.gap = 2, right = FALSE)
  # out$fstat <- fstat
  
  return(invisible(out))
} 