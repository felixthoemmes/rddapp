#' Cutoff Sensitivity Simulation for Regression Discontinuity
#'
#' \code{rd_sens_cutoff} refits the supplemented model with varying cutoff(s). 
#' Other estimation parameters, such as the automatically calculated bandwidth, are held constant.
#' 
#' @param object An object returned by \code{rd_est} or \code{rd_impute}.
#' @param cutoffs A numeric vector of cutoff values to be used in the refitting 
#'   of an \code{rd} object.
#'
#' @return A dataframe contains the estimate \code{est} and standard error \code{se} 
#'   for each cutoff values (\code{A1}). Column \code{A1} contains varying cutoffs 
#'   on the assignment variable.
#'
#' @export
#'
#' @examples
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' rd <- rd_est(y ~ x | cov)
#' rd_sens_cutoff(rd, seq(-.5, .5, length.out = 10))

rd_sens_cutoff <- function(object, cutoffs) {
  if (class(object) != "rd") 
    stop("Not an object of class rd.")
  
  sim_results <- lapply(cutoffs, 
    function(cutoff) {
      object$call$cutpoint <- cutoff
      object$call$est.cov <- FALSE
      object$call$bw <- object$bw["Opt"]
      new_model <- eval.parent(object$call, 3)
       
      return(
        data.frame(
          est = new_model$est, 
          se = new_model$se, 
          A1 = cutoff, 
          model = c("linear", "quadratic", "cubic", "optimal", "half", "double"), 
          stringsAsFactors = FALSE, 
          row.names = 1:6)
      )
    }
  )

  combined_sim_results <- do.call(rbind.data.frame, sim_results)
  original_results <- data.frame(
    est = object$est, 
    se = object$se, 
    A1 = if (is.null(object$call$cutpoint)) 0 else eval.parent(object$call$cutpoint), 
    model = c("linear", "quadratic", "cubic", "optimal", "half", "double"), 
    stringsAsFactors = FALSE, 
    row.names = 1:6)
    
  return(rbind(combined_sim_results, original_results))
}

