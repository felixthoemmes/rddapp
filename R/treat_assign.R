#' Treatment Assignment for Regression Discontinuity
#' 
#' \code{treat_assign} computes the treatment variable, \code{t}, based on the cutoff of 
#' assignment variable, \code{x}.
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::treat_assign().
#'
#' @param x A numeric vector containing the assignment variable, \code{x}.
#' @param cut A numeric value containing the cutpoint at which assignment to the treatment is determined. The default is 0.
#' @param t.design A string specifying the treatment option according to design.
#'   Options are \code{"g"} (treatment is assigned if \code{x} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x} is less than or equal to its cutoff).
#'   The default is \code{"l"}.
#'
#' @return \code{treat_assign} returns the treatment variable as a vector according to the design,
#'   where 1 means the treated group, and 0 means the control group.

treat_assign <- function(x, cut = 0, t.design = "l") {
  if(t.design == "geq")
    return(as.integer(x >= cut))
  if(t.design == "g")
    return(as.integer(x > cut))
  if(t.design == "leq")
    return(as.integer(x <= cut))
  if(t.design == "l")
    return(as.integer(x < cut))
  stop("Treatment design must be one of 'g', 'geq', 'l', 'leq'.")
}


