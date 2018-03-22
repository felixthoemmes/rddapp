#' Bandwidth Sensitivity Simulation for Multivariate Regression Discontinuity
#'
#' \code{mrd_sens_bw} refits the supplemented model with varying bandwidth. 
#' Other estimation parameters are held constant.
#'
#' @param object An object returned by \code{mrd_est} or \code{mrd_impute}. 
#' @param approach A string of the approaches to be refitted, 
#'   choosing from \code{c("center", "univ1", "univ2")}.
#' @param bws A positive numeric vector of bandwidth for refitting an \code{mrd} object.  
#'
#' @return A dataframe which contains the estimate \code{est} and standard error \code{se} 
#'   for each supplemented bandwidth.
#' 
#' @export
#'
#' @examples
#' x1 <- runif(1000, -1, 1)
#' x2 <- rnorm(1000, 10, 2)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x1 + 1 * x2 + 3 * cov + 10 * (x1 >= 0) + 5 * (x2 >= 10) + rnorm(1000)
#' mrd <- mrd_est(y ~ x1 + x2 | cov, cutpoint = c(0, 10), t.design = c("geq", "geq"))
#' mrd_sens_bw(mrd, approach = "univ1", bws = seq(0.1, 1, length.out = 5))

mrd_sens_bw <- function(object, approach = c("center", "univ1", "univ2"), bws) {
  if (class(object) != "mrd") 
    stop("Not an object of class mrd.")
  if (any(bws <= 0)) 
    stop("Bandwidth must be positive.")
  if (!approach %in% c("center", "univ1", "univ2")) 
    stop("Approach is not defined.")
  if (approach == "center" & is.null(object$center)) 
    stop("mrd object was not estimated uing the centering approach.")
  if (approach %in% c("univ1", "univ2") & is.null(object$univ)) 
    stop("mrd object was not estimated uing the univariate approach.")
  
  sim_results <- lapply(bws, 
    function(bw) {  
      object$call$bw <- bw
      object$call$est.cov <- FALSE
      object$call$method <- switch(approach, center = 'center', 'univ')

      new_model <- eval.parent(object$call, 3)

      switch(approach,
        center = data.frame(
          est = new_model$center$tau_MRD$est["Usr"], 
          # Wang: used index here because the returned array's names are constant "Z" or "Tr" 
          se = new_model$center$tau_MRD$se["Usr"], 
          bw = bw,
          model = paste("center-usr"), 
          stringsAsFactors = FALSE),
        univ1 = data.frame(
          est = new_model$univ$tau_R$est["Usr"], 
          se = new_model$univ$tau_R$se["Usr"], 
          bw = bw,
          model = paste("univ1-usr"), 
          stringsAsFactors = FALSE),
        univ2 = data.frame(
          est = new_model$univ$tau_M$est["Usr"], 
          se = new_model$univ$tau_M$se["Usr"], 
          bw = bw,
          model = paste("univ2-usr"), 
          stringsAsFactors = FALSE)
      )
    }
  )
  
  combined_sim_results <- do.call(rbind.data.frame, sim_results)
  original_result <- switch(approach,
    center = data.frame(
      est = object$center$tau_MRD$est["Opt"], 
      se = object$center$tau_MRD$se["Opt"], 
      bw = object$center$tau_MRD$bw["Opt"], 
      model = paste("center-origin"), 
      stringsAsFactors = FALSE),
    univ1 = data.frame(
      est = object$univ$tau_R$est["Opt"], 
      se = object$univ$tau_R$se["Opt"], 
      bw = object$univ$tau_R$bw["Opt"], 
      model = paste("univ1-origin"), 
      stringsAsFactors = FALSE),
    univ2 = data.frame(
      est = object$univ$tau_M$est["Opt"], 
      se = object$univ$tau_M$se["Opt"], 
      bw = object$univ$tau_M$bw["Opt"], 
      model = paste("univ2-origin"), 
      stringsAsFactors = FALSE)
  )
  
  return(rbind(combined_sim_results, original_result))
}