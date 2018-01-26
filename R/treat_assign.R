#' Treatment Assignment for Regression Discontinuity
#' 
#' \code{treat_assign} computes the treatment variable T based on the cutoff of 
#' assignment variables X.
#'
#' @param x The vector of assignment variable X.
#' @param cut The cutoff of assignment variables X.
#' @param t.design The treatment option according to design.
#'   The entry is for X: \code{"g"} means treatment is assigned 
#'   if X is greater than its cutoff, \code{"geq"} means treatment is assigned 
#'   if X is greater than or equal to its cutoff, \code{"l"} means treatment is assigned 
#'   if X is less than its cutoff, \code{"leq"} means treatment is assigned 
#'   if X is less than or equal to its cutoff.
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


