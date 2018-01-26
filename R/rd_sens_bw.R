#' Bandwidth Sensitivity Simulation for Regression Discontinuity
#'
#' \code{rd_sens_bw} refits the supplemented model with varying bandwidth. 
#' Other estimation parameters are held constant.
#' 
#' @param object An object returned by \code{rd_est} or \code{rd_impute}. 
#' @param bws A positive numeric vector of bandwidth for refitting an \code{rd} object.  
#'
#' @return A dataframe which contains the estimate \code{est} and standard error \code{se} 
#'   for each supplemented bandwidth.
#'
#' @export
#'
#' @examples
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' rd <- rd_est(y ~ x | cov)
#' rd_sens_bw(rd, bws = seq(.1, 1, length.out = 5))

rd_sens_bw <- function(object, bws) {
  if (class(object) != "rd") 
    stop("Not an object of class rd.")
  
  sim_results <- lapply(bws, 
    function(bw) {
      object$call$bw <- bw
      object$call$est.cov <- FALSE
      new_model <- eval.parent(object$call, 3)
       
      return(
        data.frame(
          est = new_model$est["Usr"], 
          se = new_model$se["Usr"], 
          bw = bw, 
          model = c("usr"), 
          stringsAsFactors = FALSE)
      )
    }
  )
  
  combined_sim_results <- do.call(rbind.data.frame, sim_results)
  original_result <- data.frame(
    est = object$est["Opt"], 
    se = object$se["Opt"], 
    bw = object$bw["Opt"], 
    model = c("origin"), 
    stringsAsFactors = FALSE)
      
  return(rbind(combined_sim_results, original_result))
}