#' Assignment Centering for Multivariate Frontier Regression Discontinuity
#' 
#' \code{var_center} computes the univariate assignment variable, \code{x} based on the cutoffs of 
#'  two assignment variables: \code{x1} and \code{x2}.
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::var_center().
#'
#' @param x Data frame or matrix of two assignment variables,
#'   where the first column is \code{x1} and the second column is \code{x2}.
#' @param cut A numeric vector of length 2 containing the cutpoints at which assignment to the treatment is determined.
#'   The default is \code{c(0, 0)}.
#' @param t.design A character vector of length 2 specifying the treatment option according to design.
#'   The first entry is for \code{x1} and the second entry is for \code{x2}. Options are  
#'   \code{"g"} (treatment is assigned if \code{x1} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x1} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x1} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x1} is less than or equal to its cutoff).
#'   The same options are available for \code{x2}.
#' @param t.plot A logical value inidcating whether to calculate the univariate treatment variable, \code{t}, and make a plot. The default is \code{FALSE}. 
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


