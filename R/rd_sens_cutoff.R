#' Cutoff Sensitivity Simulation for Regression Discontinuity
#'
#' \code{rd_sens_cutoff} refits the supplied model with varying cutoff(s). 
#' All other aspects of the model, such as the automatically calculated bandwidth, are held constant.
#' 
#' @param object An object returned by \code{rd_est} or \code{rd_impute}.
#' @param cutoffs A numeric vector of cutoff values to be used for refitting 
#'   an \code{rd} object.
#'
#' @return \code{rd_sens_cutoff} returns a dataframe containing the estimate \code{est} and standard error \code{se} 
#'   for each cutoff value (\code{A1}). Column \code{A1} contains varying cutoffs 
#'   on the assignment variable. The \code{model} column contains the parametric model (linear, quadratic, or cubic) or 
#'   non-parametric bandwidth setting (Imbens-Kalyanaraman 2012 optimal, half, or double) used for estimation.
#'
#' @references Imbens, G., Kalyanaraman, K. (2012). 
#'   Optimal bandwidth choice for the regression discontinuity estimator. 
#'   The Review of Economic Studies, 79(3), 933-959.
#'   \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
#'
#' @export
#'
#' @examples
#' set.seed(12345)
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' rd <- rd_est(y ~ x | cov, t.design = "geq")
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

