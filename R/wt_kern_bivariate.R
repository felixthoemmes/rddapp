#' Bivariate Kernel Weight Calculation
#' 
#' \code{wt_kern_bivariate} calculates the appropriate weights for two variables for
#'    Multivariate Frontier Regression Discontinuity Estimation with nonparametric implementation.
#'    Kernel weights are calculated based on the L1 distance of the two variables from the frontiers.
#' @param X1 The input x1 values for the first vector. 
#'   This variable represents the axis along which kernel weighting should be performed.
#' @param X2 The input x2 values for the second vector. \code{X2} has the same length as \code{X1}.
#'   This variable represents the axis along which kernel weighting should be performed.   
#' @param center1 A numeric value specifying the point from which distances should be calculated for the first vector, \code{X1}.
#' @param center2 A numeric value specifying the point from which distances should be calculated for the second vector, \code{X2}.
#' @param bw A numeric vector specifying the bandwidths for each of three effects models
#' @param kernel A string indicating which kernel to use. Options are \code{"triangular"} 
#'   (default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
#'   \code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.
#' @param t.design A character vector of length 2 specifying the treatment option according to design.
#'   The first entry is for \code{x1} and the second entry is for \code{x2}. Options are  
#'   \code{"g"} (treatment is assigned if \code{x1} is greater than its cutoff),
#'   \code{"geq"} (treatment is assigned if \code{x1} is greater than or equal to its cutoff),
#'   \code{"l"} (treatment is assigned if \code{x1} is less than its cutoff),
#'   and \code{"leq"} (treatment is assigned if \code{x1} is less than or equal to its cutoff).
#'   The same options are available for \code{x2}.
#'
#' @return \code{wt_bivariate_kern} returns three vectors of weights and distances with length equal to that of the \code{X1} and \code{X2} input.
#'   The first and second weights and distances are calculated with respect to all frontiers of different treatments.
#'   The third weight and distance are calculated with respect to the overall frontier of treatment versus
#'   non-treatment.
#'   
#' @include treat_assign.R

wt_kern_bivariate <- function(X1, X2, center1, center2, bw, kernel = "triangular",
                              t.design = NULL) {
  if (is.null(t.design)){
    stop("Specify t.design.")
  }
  
  if (length(bw) == 1){
    bw = rep(bw, 3)
  }
  
  dist1.1 <- abs(X1 - center1) / bw[1]
  dist1.2 <- abs(X2 - center2) / bw[1]
  dist2.1 <- abs(X1 - center1) / bw[2]
  dist2.2 <- abs(X2 - center2) / bw[2]
  dist3.1 <- abs(X1 - center1) / bw[3]
  dist3.2 <- abs(X2 - center2) / bw[3]
  # synthesize dist1 and dist2
  x1_tr <- treat_assign(X1, center1, t.design[1])
  x2_tr <- treat_assign(X2, center2, t.design[2])
  tr1 <- ifelse(is.na(X1) | is.na(X2), NA, ifelse(x1_tr & !x2_tr, 1, 0))
  tr2 <- ifelse(is.na(X1) | is.na(X2), NA, ifelse(!x1_tr & x2_tr, 1, 0))
  trb <- ifelse(is.na(X1) | is.na(X2), NA, ifelse(x1_tr & x2_tr, 1, 0))
  tr <- ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
  
  M1 = abs(matrix(c(dist1.1, dist1.2), ncol = 2))
  M2 = abs(matrix(c(dist2.1, dist2.2), ncol = 2))
  M3 = abs(matrix(c(dist3.1, dist3.2), ncol = 2))
  distAll1 = apply(M1, 1, min)
  distAll2 = apply(M2, 1, min)
  distTr  = apply(M3, 1, min)
  
  distTr[ifelse(!is.na(tr1) & tr1==1, T, F)] = dist3.1[ifelse(!is.na(tr1) & tr1==1, T, F)]
  distTr[ifelse(!is.na(tr2) & tr2==1, T, F)] = dist3.2[ifelse(!is.na(tr2) & tr2==1, T, F)]
  distTr[ifelse(!is.na(trb) & trb==1, T, F)] = (dist3.1 + dist3.2)[ifelse(!is.na(trb) & trb==1, T, F)]     
  
  if (kernel == "triangular") {
    wAll1 <- (1 - abs(distAll1))
    wAll2 <- (1 - abs(distAll2))
    wTr <- (1 - abs(distTr))
  } else if (kernel == "rectangular") {
    wAll1 <- 1/2
    wAll2 <- 1/2
    wTr <- 1/2
  } else if (kernel == "epanechnikov") {
    wAll1 <- 3/4 * (1 - distAll1^2)
    wAll2 <- 3/4 * (1 - distAll2^2)
    wTr <- 3/4 * (1 - distTr^2)
  } else if (kernel == "quartic" | kernel == "biweight") {
    wAll1 <- 15/16 * (1 - distAll1^2)^2
    wAll2 <- 15/16 * (1 - distAll2^2)^2
    wTr <- 15/16 * (1 - distTr^2)^2
  } else if (kernel == "triweight") {
    wAll1 <- 35/32 * (1 - distAll1^2)^3
    wAll2 <- 35/32 * (1 - distAll2^2)^3
    wTr <- 35/32 * (1 - distTr^2)^3
  } else if (kernel == "tricube") {
    wAll1 <- 70/81 * (1 - abs(distAll1)^3)^3
    wAll2 <- 70/81 * (1 - abs(distAll2)^3)^3
    wTr <- 70/81 * (1 - abs(distTr)^3)^3
  } else if (kernel == "gaussian") {
    wAll1 <- 1/sqrt(2 * pi) * exp(-1/2 * distAll1^2)
    wAll2 <- 1/sqrt(2 * pi) * exp(-1/2 * distAll2^2)
    wTr <- 1/sqrt(2 * pi) * exp(-1/2 * distTr^2)
  } else if (kernel == "cosine") {
    wAll1 <- pi/4 * cos(pi/2 * distAll1)
    wAll2 <- pi/4 * cos(pi/2 * distAll2)
    wTr <- pi/4 * cos(pi/2 * distTr)
  } else {
    stop("Invalid kernel selection.")
  }
    
  wAll1 <- ifelse(distAll1 > 1 & kernel != "gaussian", 0, wAll1)
  wAll2 <- ifelse(distAll2 > 1 & kernel != "gaussian", 0, wAll2)
  wTr <- ifelse(distTr > 1 & kernel != "gaussian", 0, wTr)
  wAll1 <- wAll1 / sum(wAll1, na.rm = TRUE)
  wAll2 <- wAll2 / sum(wAll2, na.rm = TRUE)
  wTr <- wTr / sum(wTr, na.rm = TRUE)
  
  return(list(wAll1 = wAll1, wAll2 = wAll2, wTr = wTr, 
              distAll1 = distAll1, distAll2 = distAll2, distTr = distTr))
} 
