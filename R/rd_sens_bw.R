#' Bandwidth Sensitivity Simulation for Regression Discontinuity
#'
#' \code{rd_sens_bw} refits the supplied model with varying bandwidths. 
#' All other aspects of the model are held constant.
#' 
#' @param object An object returned by \code{rd_est} or \code{rd_impute}. 
#' @param bws A positive numeric vector of the bandwidths for refitting an \code{rd} object.
#'
#' @return \code{rd_sens_bw} returns a dataframe containing the estimate \code{est} and standard error \code{se} 
#'   for each supplied bandwidth and for the Imbens-Kalyanaraman (2012) optimal bandwidth, \code{bw}, 
#'   and for each supplied approach, \code{model}.  Approaches are either user
#'   specified (\code{"usr"}) or based on the optimal bandwidth (\code{"origin"}).
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
          bw = bw,
          est = new_model$est["Usr"], 
          se = new_model$se["Usr"], 
          model = c("usr"), 
          stringsAsFactors = FALSE)
      )
    }
  )
  
  combined_sim_results <- do.call(rbind.data.frame, sim_results)
  original_result <- data.frame(
    bw = object$bw["Opt"],
    est = object$est["Opt"], 
    se = object$se["Opt"], 
    model = c("origin"), 
    stringsAsFactors = FALSE)
      
  return(rbind(combined_sim_results, original_result))
}