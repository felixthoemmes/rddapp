#' Kernel Weight Calculation
#' 
#' \code{wt_kern} calculates the appropriate kernel weights for a vector. 
#' This is useful when, for instance, one wishes to perform local regression.
#' It is based on the \code{kernelwts} function in the "rdd" package. 
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::wt_kern().
#'
#' @param X A numeric vector containing the the input \code{X} values. This variable represents the axis along which kernel weighting 
#'   should be performed.
#' @param center A numeric value specifying the point from which distances should be calculated.
#' @param bw A numeric value specifying the bandwidth.
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.
#'
#' @return \code{wt_kern} returns a vector of weights with length equal to that of the \code{X} input 
#'   (one weight per element of \code{X}).
#'   
#' @references Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
#'   version 0.57. https://CRAN.R-project.org/package=rdd

wt_kern <- function(X, center, bw, kernel = "triangular") {
  dist <- (X - center) / bw
  if (kernel == "triangular") {
    w <- (1 - abs(dist))
  } else if (kernel == "rectangular") {
    w <- 1/2
  } else if (kernel == "epanechnikov") {
    w <- 3/4 * (1 - dist^2)
  } else if (kernel == "quartic" | kernel == "biweight") {
    w <- 15/16 * (1 - dist^2)^2
  } else if (kernel == "triweight") {
    w <- 35/32 * (1 - dist^2)^3
  } else if (kernel == "tricube") {
    w <- 70/81 * (1 - abs(dist)^3)^3
  } else if (kernel == "gaussian") {
    w <- 1/sqrt(2 * pi) * exp(-1/2 * dist^2)
  } else if (kernel == "cosine") {
    w <- pi/4 * cos(pi/2 * dist)
  } else {
    stop("Invalid kernel selection.")
  }
  w <- ifelse(abs(dist) > 1 & kernel != "gaussian", 0, w)
  w <- w / sum(w)
  return(w)
} 
