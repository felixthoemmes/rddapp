#' Print the Multivariate Frontier Regression Discontinuity
#' 
#' \code{print.mfrd} prints a very basic summary of the multivariate frontier regression 
#' discontinuity. It is based on the \code{print.RD} function in the "rdd" package. 
#' 
#' @method print mfrd
#'
#' @param x An \code{mfrd} object, typically the result of \code{\link{mfrd_est}}.
#' @param digits A non-negative integer specifying the number of digits to print.
#'   The default is \code{max(3, getOption("digits") - 3)}.
#' @param ... Additional arguments passed to \code{print}.
#' 
#' @references Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
#'    version 0.57. https://CRAN.R-project.org/package=rdd
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
