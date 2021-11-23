#' rddapp: A package for regression discontinuity designs (RDDs).
#'
#' The rddapp package provides a set of functions for the analysis 
#' of the regression-discontinuity design (RDD). The three main parts are:
#' estimation of effects of interest, power analysis, and assumption checks.
#' 
#' @section Estimation:
#' A variety of designs can be estimated in various ways. 
#' The single-assignment RDD (both sharp and fuzzy) can be analyzed using
#' both a parametric (global) or non-parametric (local) approach. 
#' The multiple-assignment RDD (both sharp and fuzzy) can be analyzed using both parametric and 
#' non-parametric estimation. The analysis choices are further to use estimate effects based on
#' univariate scaling, the centering approach, or the frontier approach. The frontier approach
#' can currently only be estimated using parametric regression with bootstrapped standard errors. 
#'
#' @section Power analysis:
#' Statistical power can be be estimated for both the single- and multiple-assignment RDD,
#' (both sharp and fuzzy), including all parametric and non-parametric estimators mentioned in 
#' the estimation section. All power analyses are based on a simulation approach, which means that 
#' the user has to provide all necessary parameters for a data-generating model. 
#'
#' @section Assumption checks:
#' An important part of any RDD are checks of underlying assumptions. The package provides users 
#' with the option to estimate McCrary's sorting test (to identify violations of assignment rules), 
#' checks of discontinuities of other baseline covariates, along with sensitivity checks of the 
#' chosen bandwidth parameter for non-parametric models, and so-called placebo tests, that examine
#' the treatment effect at other cut-points along the assignment variable. 
#'
#' @name rddapp-package
#'
#' @aliases rddapp
#'
#' @docType package
#'
#' @title Regression Discontinuity Design Application
#'
#' @author Ze Jin \email{zj58@cornell.edu}, 
#'   Wang Liao \email{wl483@cornell.edu},
#'   Irena Papst \email{ip98@cornell.edu},
#'   Wenyu Zhang \email{wz258@cornell.edu},
#'   Kimberly Hochstedler \email{kah343@cornell.edu},
#'   Felix Thoemmes, \email{fjt36@cornell.edu}

NULL 
