#' Print the Regression Discontinuity
#' 
#' \code{print.rd} prints a very basic summary of the regression discontinuity.
#' It is based on the \code{\link[rdd]{print.RD}} function in the \pkg{rdd} package. 
#' 
#' @method print rd
#'
#' @param x An \code{rd} object, typically the result of \code{\link{rd_est}}.
#' @param digits The number of digits to print.
#' @param ... Additional arguments.
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
