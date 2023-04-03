#' Cutoff Sensitivity Simulation for Multivariate Regression Discontinuity
#'
#' \code{mrd_sens_cutoff} refits the supplied model with varying cutoff(s). 
#' All other aspects of the model, such as the automatically calculated bandwidth, are held constant.
#' 
#' @param object An object returned by \code{mrd_est} or \code{mrd_impute}. 
#' @param cutoffs A two-column numeric matrix of paired cutoff values 
#'   to be used for refitting an \code{mrd} object. The first column corresponds
#'   to cutoffs for \code{x1} and the second column corresponds to cutoffs
#'   for \code{x2}. 
#'
#' @return \code{mrd_sens_cutoff} returns a dataframe containing the estimate \code{est} and standard error \code{se} 
#'   for each pair of cutoffs (\code{A1} and \code{A2}) and for each \code{model}. \code{A1} contains varying cutoffs 
#'   for assignment 1 and \code{A2} contains varying cutoffs for assignment 2.
#'   The \code{model} column contains the approach (either centering, univariate 1, or univariate 2)
#'   for determining the cutoff and the parametric model (linear, quadratic, or cubic) or 
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
#' x1 <- runif(10000, -1, 1)
#' x2 <- rnorm(10000, 10, 2)
#' cov <- rnorm(10000)
#' y <- 3 + 2 * x1 + 1 * x2 + 3 * cov + 10 * (x1 >= 0) + 5 * (x2 >= 10) + rnorm(10000)
#' # front.bw arugment was supplied to speed up the example
#' # users should choose appropriate values for front.bw
#' mrd <- mrd_est(y ~ x1 + x2 | cov,
#'                cutpoint = c(0, 10), t.design = c("geq", "geq"), front.bw = c(1,1,1))
#' mrd_sens_cutoff(mrd, expand.grid(A1 = seq(-.5, .5, length.out = 3), A2 = 10))

mrd_sens_cutoff <- function(object, cutoffs) {
  if (!inherits(object, "mrd"))
    stop("Not an object of class mrd.")
  if (is.null(object$call$cutpoint)) 
    object$call$cutpoint <- quote(c(0,0))
  
  sim_results <- apply(cutoffs, 1, 
    function(cutoff) {
      if (is.null(object$center) & is.null(object$univ)) 
        stop("mrd object was not estimated uing the centering or univariate method.")
      
      object$call$cutpoint <- cutoff
      object$call$est.cov <- FALSE
      
      result_center <- if (!is.null(object$center)) {
        object$call$bw <- object$center$tau_MRD$bw["Opt"]
        object$call$method <- "center"
        new_model <- eval.parent(object$call, 3)
        data.frame(
          est = new_model$center$tau_MRD$est, 
          se = new_model$center$tau_MRD$se, 
          A1 = cutoff[1], 
          A2 = cutoff[2], 
          model = paste("center", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
            sep = "-"), 
          stringsAsFactors = FALSE, 
          row.names = 1:6)
      } 
      
      result_univ <- if (!is.null(object$univ)) {
        object$call$bw <- object$univ$tau_R$bw["Opt"]
        object$call$method <- "univ"
        new_model <- eval.parent(object$call, 3)
        rbind(
          data.frame(
            est = new_model$univ$tau_R$est, 
            se = new_model$univ$tau_R$se, 
            A1 = cutoff[1], 
            A2 = cutoff[2], 
            model = paste("univ1", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
              sep = "-"), 
            stringsAsFactors = FALSE, 
            row.names = 1:6),
          data.frame(
            est = new_model$univ$tau_M$est, 
            se = new_model$univ$tau_M$se, 
            A1 = cutoff[1], 
            A2 = cutoff[2], 
            model = paste("univ2", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
              sep = "-"), 
            stringsAsFactors = FALSE, 
            row.names = 1:6)
        )
      }
      
      return(rbind(result_center, result_univ))
    }
  )
  
  combined_sim_results <- do.call(rbind.data.frame, sim_results)
  original_result_center <- if (!is.null(object$center)) {
    data.frame(
      est = object$center$tau_MRD$est, 
      se = object$center$tau_MRD$se, 
      A1 = eval(object$call$cutpoint)[1], 
      A2 = eval(object$call$cutpoint)[2], 
      model = paste("center", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
        sep = "-"), 
      stringsAsFactors = FALSE, 
      row.names = 1:6)
  } 
  
  original_result_univ <- if (!is.null(object$univ)) {
    rbind(
      data.frame(
        est = object$univ$tau_R$est, 
        se = object$univ$tau_R$se, 
        A1 = eval(object$call$cutpoint)[1], 
        A2 = eval(object$call$cutpoint)[2], 
        model = paste("univ1", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
          sep = "-"), 
        stringsAsFactors = FALSE, 
        row.names = 1:6),
      data.frame(
        est = object$univ$tau_M$est, 
        se = object$univ$tau_M$se, 
        A1 = eval(object$call$cutpoint)[1], 
        A2 = eval(object$call$cutpoint)[2], 
        model = paste("univ2", c("linear", "quadratic", "cubic", "optimal", "half", "double"),
          sep = "-"), 
        stringsAsFactors = FALSE, 
        row.names = 1:6)
    )
  }  
  
  return(rbind(combined_sim_results, original_result_center, original_result_univ))
}
