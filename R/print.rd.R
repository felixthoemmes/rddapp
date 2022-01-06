#' Print the Regression Discontinuity
#' 
#' \code{print.rd} prints a basic summary of the regression discontinuity.
#' \code{print.rd} is based on the \code{print.RD} function in the "rdd" package. 
#' 
#' @method print rd
#'
#' @param x An \code{rd} object, typically the result of \code{\link{rd_est}}.
#' @param digits A non-negative integer specifying the number of digits to print.
#' @param ... Additional arguments passed to \code{print}.
#' 
#' @references Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
#'    version 0.57. https://CRAN.R-project.org/package=rdd
#'
#' @include rd_est.R
#'
#' @export

print.rd <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", 
    sep = "")
  
  cat("Coefficients:\n")
  print.default(format(x$est, digits = digits), print.gap = 2, quote = FALSE)
  cat("\n")
  
  invisible(x)
} 
