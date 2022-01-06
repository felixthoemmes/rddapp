#' Summarize the Multiple Imputation of Multivariate Regression Discontinuity
#' 
#' \code{summary.mrdi} is a \code{summary} method for class \code{"mrdi"}.
#' It is based on \code{summary.RD} function in the "rdd" package.
#' 
#' @method summary mrdi
#' 
#' @param object An object of class \code{"mrdi"}, usually a result of a call to 
#' \code{\link{mrd_impute}} with \code{"front"} method.
#' @param level A numeric value between 0 and 1 specifying the confidence level for confidence intervals. The default is 0.95.
#' @param digits A non-negative integer specifying the number of digits to display.
#' @param ... Additional arguments passed to \code{summary}.
#' 
#' @return \code{summary.mrdi} returns a list which has the following components:
#' \item{coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the complete model.}
#' \item{ht_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the heterogeneous treatment model.}
#' \item{t_coefficients}{A matrix containing estimates and confidence intervals (if applicable) 
#' for the treatment only model.}
#' 
#' @references Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
#'    version 0.57. https://CRAN.R-project.org/package=rdd
#' 
#' @importFrom stats residuals
#'
#' @include mrd_impute.R
#' 
#' @export

summary.mrdi <- function(object, level = 0.95, digits = max(3, getOption("digits") - 3), ...) {
  call.copy <- object$call
  if ("data" %in% names(call.copy) && length(call.copy$data) > 1) {
    call.copy$data <- "(.)"
  }
  if ("subset" %in% names(call.copy) && length(call.copy$subset) > 1) {
    call.copy$subset <- "(.)"
  }
  
  cat("\n")
  cat("Call:\n")
  # print(object$call)
  print(call.copy)
  cat("\n")
  
  obj.front = object$front$tau_MRD
  n = dim(obj.front$est)
  if (!all(is.na(obj.front$z))){
    alpha <- 1 - level
    lower.CL <- obj.front$est - qt(1-alpha/2, df = obj.front$df) * obj.front$se
    upper.CL <- obj.front$est + qt(1-alpha/2, df = obj.front$df) * obj.front$se
    
    stars <- matrix(NA, n[1], n[2])
    rownames(stars) <- rownames(obj.front$est)
    colnames(stars) <- colnames(obj.front$est)
    
    for (i in 1:n[1]) {
      for (j in 1:n[2]){
        stars[i,j] <- if (is.na(obj.front$p[i,j])) 
          " " else if (obj.front$p[i,j] < 0.001) 
            "***" else if (obj.front$p[i,j] < 0.01) 
              "**" else if (obj.front$p[i,j] < 0.05) 
                "*" else if (obj.front$p[i,j] < 0.1) 
                  "." else " "        
      }
    }
  }
  
  if (!all(is.na(obj.front$z))){
    out.all.param <- cbind(obj.front$est['Param',], obj.front$se['Param',], lower.CL['Param',], upper.CL['Param',],
                           obj.front$z['Param',], obj.front$df['Param',], obj.front$p['Param',])
    outmat.param <- cbind(apply(out.all.param, 2, function(x) format(x, digits = digits)), 
                          " " = stars['Param',])
    
    out.all.bw <- cbind(obj.front$est['bw',], obj.front$se['bw',], lower.CL['bw',], upper.CL['bw',],
                           obj.front$z['bw',], obj.front$df['bw',], obj.front$p['bw',])
    outmat.bw <- cbind(apply(out.all.bw, 2, function(x) format(x, digits = digits)), 
                          " " = stars['bw',])
    
    out.all.half <- cbind(obj.front$est['Half-bw',], obj.front$se['Half-bw',], lower.CL['Half-bw',], upper.CL['Half-bw',],
                        obj.front$z['Half-bw',], obj.front$df['Half-bw',], obj.front$p['Half-bw',])
    outmat.half <- cbind(apply(out.all.half, 2, function(x) format(x, digits = digits)), 
                       " " = stars['Half-bw',])
    
    out.all.double <- cbind(obj.front$est['Double-bw',], obj.front$se['Double-bw',], lower.CL['Double-bw',], upper.CL['Double-bw',],
                        obj.front$z['Double-bw',], obj.front$df['Double-bw',], obj.front$p['Double-bw',])
    outmat.double <- cbind(apply(out.all.double, 2, function(x) format(x, digits = digits)), 
                       " " = stars['Double-bw',])
    
    colnames(out.all.param) = colnames(out.all.bw) = colnames(out.all.half) = colnames(out.all.double) <- 
      c("Estimate", "Std. Error", "lower.CL", "upper.CL", "t value", "df", "Pr(>|t|)")
    colnames(outmat.param) = colnames(outmat.bw) = colnames(outmat.half) = colnames(outmat.double) <-
      c("Estimate", "Std. Error", "lower.CL", "upper.CL", "t value", "df", "Pr(>|t|)", "")
  } else {
    out.all.param <- matrix(obj.front$est['Param',], ncol = 1)
    out.all.bw <- matrix(obj.front$est['bw',], ncol = 1)
    out.all.half <- matrix(obj.front$est['Half-bw',], ncol = 1)
    out.all.double <- matrix(obj.front$est['Double-bw',], ncol = 1)
    rownames(out.all.param) = rownames(out.all.bw) = rownames(out.all.half) = 
      rownames(out.all.double) <- colnames(obj.front$est)
    colnames(out.all.param) = colnames(out.all.bw) = colnames(out.all.half) =
      colnames(out.all.double) <- c("Estimate")
    
    outmat.param <- apply(out.all.param, 2, function(x) format(x, digits = digits))
    outmat.bw <- apply(out.all.bw, 2, function(x) format(x, digits = digits))
    outmat.half <- apply(out.all.half, 2, function(x) format(x, digits = digits))
    outmat.double <- apply(out.all.double, 2, function(x) format(x, digits = digits))
  }
  
  cat("Estimates for Complete Model:\n")
  
  if (!all(is.na(obj.front$z))){
    out <- list('Param' = out.all.param[1:3,], 'bw' = out.all.bw[1:3,],
                'Half-bw' = out.all.half[1:3,], 'Double-bw' = out.all.double[1:3,])
    cat("Parametric:\n")
    print.default(outmat.param[1:3,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[1], digits = digits), ":\n",
              sep = ''))
    print.default(outmat.bw[1:3,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(outmat.half[1:3,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(outmat.double[1:3,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    out <- list('Param' = matrix(out.all.param[1:3,]), 'bw' = matrix(out.all.bw[1:3,]),
                'Half-bw' = matrix(out.all.half[1:3,]), 'Double-bw' = matrix(out.all.double[1:3,]))
    colnames(out$'Param') = colnames(out$'bw') = colnames(out$'Half-bw') = colnames(out$'Double-bw') <- colnames(out.all.param)
    rownames(out$'Param') = rownames(out$'bw') = rownames(out$'Half-bw') = rownames(out$'Double-bw') <- rownames(out.all.param)[1:3]
    cat("Parametric:\n")
    print.default(matrix(outmat.param[1:3,], dimnames = list(rownames(out$'Param'), colnames(out$'Param'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[1], digits = digits), ":\n",
              sep = ''))
    print.default(matrix(outmat.bw[1:3,], dimnames = list(rownames(out$'bw'), colnames(out$'bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.half[1:3,], dimnames = list(rownames(out$'Half-bw'), colnames(out$'Half-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.double[1:3,], dimnames = list(rownames(out$'Double-bw'), colnames(out$'Double-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  cat("Estimates for Heterogeneous Treatment Model:\n")
  
  if (!all(is.na(obj.front$z))){
    ht_out <- list('Param' = out.all.param[4:6,], 'bw' = out.all.bw[4:6,],
                   'Half-bw' = out.all.half[4:6,], 'Double-bw' = out.all.double[4:6,])
    cat("Parametric:\n")
    print.default(outmat.param[4:6,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[2], digits = digits), ":\n",
              sep = ''))
    print.default(outmat.bw[4:6,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(outmat.half[4:6,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(outmat.double[4:6,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    ht_out <- list('Param' = matrix(out.all.param[4:6,]), 'bw' = matrix(out.all.bw[4:6,]),
                'Half-bw' = matrix(out.all.half[4:6,]), 'Double-bw' = matrix(out.all.double[4:6,]))
    colnames(ht_out$'Param') = colnames(ht_out$'bw') = colnames(ht_out$'Half-bw') = colnames(ht_out$'Double-bw') <- colnames(out.all.param)
    rownames(ht_out$'Param') = rownames(ht_out$'bw') = rownames(ht_out$'Half-bw') = rownames(ht_out$'Double-bw') <- rownames(out.all.param)[4:6]
    cat("Parametric:\n")
    print.default(matrix(outmat.param[4:6,], dimnames = list(rownames(out$'Param'), colnames(out$'Param'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[2], digits = digits), ":\n",
              sep = ''))
    print.default(matrix(outmat.bw[4:6,], dimnames = list(rownames(out$'bw'), colnames(out$'bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.half[4:6,], dimnames = list(rownames(out$'Half-bw'), colnames(out$'Half-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.double[4:6,], dimnames = list(rownames(out$'Double-bw'), colnames(out$'Double-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  cat("Estimates for Treatment Only Model:\n")
  
  if (!all(is.na(obj.front$z))){
    t_out <- list('Param' = out.all.param[7:9,], 'bw' = out.all.bw[7:9,],
                  'Half-bw' = out.all.half[7:9,], 'Double-bw' = out.all.double[7:9,])
    cat("Parametric:\n")
    print.default(outmat.param[7:9,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[3], digits = digits), ":\n",
              sep = ''))
    print.default(outmat.bw[7:9,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(outmat.half[7:9,], quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(outmat.double[7:9,], quote = FALSE, print.gap = 2, right = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  }else{
    t_out <- list('Param' = matrix(out.all.param[7:9,]), 'bw' = matrix(out.all.bw[7:9,]),
                  'Half-bw' = matrix(out.all.half[7:9,]), 'Double-bw' = matrix(out.all.double[7:9,]))
    colnames(t_out$'Param') = colnames(t_out$'bw') = colnames(t_out$'Half-bw') = colnames(t_out$'Double-bw') <- colnames(out.all.param)
    rownames(t_out$'Param') = rownames(t_out$'bw') = rownames(t_out$'Half-bw') = rownames(t_out$'Double-bw') <- rownames(out.all.param)[7:9]
    cat("Parametric:\n")
    print.default(matrix(outmat.param[7:9,], dimnames = list(rownames(out$'Param'), colnames(out$'Param'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with bandwidth ", format(obj.front$front.bw[3], digits = digits), ":\n",
              sep = ''))
    print.default(matrix(outmat.bw[7:9,], dimnames = list(rownames(out$'bw'), colnames(out$'bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with half bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.half[7:9,], dimnames = list(rownames(out$'Half-bw'), colnames(out$'Half-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat(paste("Non-parametric with double bandwidth:\n",
              sep = ''))
    print.default(matrix(outmat.double[7:9,], dimnames = list(rownames(out$'Double-bw'), colnames(out$'Double-bw'))), 
                  quote = FALSE, print.gap = 2, right = FALSE)
    cat("\n")
  }
  
  all_out <- list(coefficients = out, ht_coefficients = ht_out, t_coefficients = t_out)
  
  return(invisible(all_out))
} 