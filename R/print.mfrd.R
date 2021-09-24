#' Print the Multivariate Frontier Regression Discontinuity
#' 
#' \code{print.mfrd} prints a very basic summary of the multivariate frontier regression 
#' discontinuity. It is based on the print.RD function in the rdd package. 
#' 
#' @method print mfrd
#'
#' @param x An \code{mfrd} object, typically the result of \code{\link{mfrd_est}}.
#' @param digits The number of digits to print.
#' @param ... Additional arguments.
#'
#' @include mfrd_est.R
#'
#' @export

print.mfrd <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", 
    sep = "")
  
  cat("Coefficients:\n")
  print.default(format(x$est, digits = digits), print.gap = 2, quote = FALSE)
  cat("\n")
  
  invisible(x)
} 
