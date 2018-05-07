#' Assignment Centering for Multivariate Frontier Regression Discontinuity
#' 
#' \code{var_center} computes the univariate assignment variable X based on the cutoffs of 
#'  two assignment variables X1, X2.
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::var_center().
#'
#' @param x Data frame or matrix of two assignment variables,
#'   where the 1st column is X1, the 2nd column is X2
#' @param cut Cutoffs of two assignment variables X1, X2.
#' @param t.design The treatment option according to design.
#'   The 1st entry is for x1: \code{"g"} means treatment is assigned 
#'   if x1 is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if x1 is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if x1 is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if x1 is less than or equal to its cutoff.
#'   The 2nd entry is for x2.
#' @param t.plot Whether calculate the univariate treatment variable T and make a plot 
#    showing T and X or not.
#'
#' @return \code{var_center} returns the univariate assignment variable as a vector 
#'   according to the design.
#'
#' @importFrom graphics text plot abline

var_center <- function(x, cut = c(0, 0), t.design = NULL, t.plot = FALSE) {
  if (dim(x)[2] != 2) {
    stop("Exactly 2 assignment variables are needed.")
  }

  if (length(cut) != 2) {
    stop("Exactly 2 cutpoints are needed.")
  }
  
  if (is.null(t.design)){
    stop("Specify t.design.")
  }

  if (!all(t.design %in% c("g", "geq", "l", "leq"))) {
    stop("Treatment design must be one of 'g', 'geq', 'l', 'leq'.")
  }

  # design of treatment comes into play
  if (t.design[1] == "g" || t.design[1] == "geq") {
    c.pos1 <- -x[, 1] + cut[1]
  } else {
    c.pos1 <- x[, 1] - cut[1]
  }

  if (t.design[2] == "g" || t.design[2] == "geq") {
    c.pos2 <- -x[, 2] + cut[2]
  } else {
    c.pos2 <- x[, 2] - cut[2]
  }      
  
  x_var <- apply(cbind(c.pos1, c.pos2), 1, min)

  if (t.plot) {
    t_var <- rep(TRUE, dim(x)[1])
    if (all(t.design %in% c("geq", "leq"))) {
      t_var[x_var > 0] <- FALSE
    } else {
      t_var[x_var >= 0] <- FALSE
    }
    
    plot(x, type = "n", xlab = "x1", ylab = "x2")
    text(x, labels = ifelse(t_var, "T", "C"), cex = 0.5)
    abline(v = cut[1], lty = 2)
    abline(h = cut[2], lty = 2)
  }
  
  return(x_var)
}


