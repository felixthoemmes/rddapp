#' Summarize the Power Analysis of Regression Discontinuity
#' 
#' \code{summary.mrdp} is a \code{summary} method for class \code{"mrdp"}
#' 
#' @method summary mrdp
#' 
#' @param object An object of class \code{"mrdp"}, usually a result of a call to \code{\link{mrd_power}}.
#' @param digits Number of digits to display.
#' @param ... Additional arguments.
#' 
#' @return \code{summary.mrdp} returns a list which has the following components:
#' \item{coefficients}{A matrix containing the mean, variance, and empirical alpha of each estimator.}
#'
#' @include mrd_power.R
#' 
#' @export

summary.mrdp <- function(object, digits = max(3, getOption("digits") - 3), ...) {

  colnames(object) = c("Repetitions", "Mean", "Std. Error", 
                       paste("alpha = ", as.numeric(colnames(object)[4:ncol(object)])*100, "%", sep = ''))
  object[, "Std. Error"] = sqrt(object[, "Std. Error"])
  
  outmat <- apply(object, 2, function(x) format(x, digits = digits))
  outmat1 <- outmat[1:2,]
  outmat2.1 <- outmat[3:4,]
  outmat2.2 <- outmat[5:6,]
  
  cat("Estimates from centering approach:\n")
  print.default(outmat1, quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  
  cat("Estimates from univariate approach:\n")
  cat("assignment variable 1:\n")
  print.default(outmat2.1, quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")
  cat("assignment variable 2:\n")
  print.default(outmat2.2, quote = FALSE, print.gap = 2, right = FALSE)
  cat("\n")

  coefficients <- list(center = unclass(object[1:2,]), 
                      univ = list(unclass(object[3:4,]), unclass(object[5:6,])))
  out <- list(coefficients = coefficients)
  
  return(invisible(out))
} 