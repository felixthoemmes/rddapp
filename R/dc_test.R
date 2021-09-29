#' McCrary Sorting Test
#' 
#' \code{dc_test} implements the McCrary (2008) sorting test.
#' It is based on the DCdensity function in the rdd package. 
#' 
#' @param runvar Numerical vector of the running variable.
#' @param cutpoint The cutpoint (defaults to 0).
#' @param bin The binwidth (defaults to \code{2*sd(runvar)*length(runvar)^(-.5)}).
#' @param bw The bandwidth to use (by default uses bandwidth selection calculation 
#'   from McCrary (2008)).
#' @param verbose Logical flag specifying whether to print diagnostic information to 
#'   the terminal (defaults to \code{TRUE}).
#' @param plot Logical flag indicating whether to plot the histogram and density estimations 
#'   (defaults to \code{TRUE}). The user may wrap this function in additional graphical options 
#'   to modify the plot.
#' @param ext.out Logical flag indicating whether to return extended output. 
#'   When \code{FALSE} (the default) \code{DCdensity} will return only the p-value of the test. 
#'   When \code{TRUE}, \code{DCdensity} will return the additional information documented below.
#' @param htest Logical flag indicating whether to return an \code{"htest"} object 
#'   compatible with base R's hypothesis test output.
#' @param level Numerical value between 0 and 1. Confidence level for confidence intervals.
#' @param digits Number of digits to display.
#' @param timeout Numerical value specifying the maximum number of seconds (defaults to 30 seconds) 
#'   expressions in the function are allowed to run. Specify \code{Inf} to run all expressions
#'   to completion.
#'
#' @return If \code{ext.out} is \code{FALSE}, only the p value will be returned. 
#'   Additional output is enabled when \code{ext.out} is \code{TRUE}. 
#'   In this case, a list will be returned with the following elements:
#' \item{theta}{The estimated log difference in heights at the cutpoint.}
#' \item{se}{The standard error of \code{theta}.}
#' \item{z}{The z statistic of the test.}
#' \item{p}{The p-value of the test. A p-value below the significance threshold indicates 
#'   that the user can reject the null hypothesis of no sorting.}
#' \item{binsize}{The calculated size of bins for the test.}
#' \item{bw}{The calculated bandwidth for the test.}
#' \item{cutpoint}{The cutpoint used.}
#' \item{data}{A dataframe for the binning of the histogram. Columns are \code{cellmp} 
#'   (the midpoints of each cell) and \code{cellval} (the normalized height of each cell).}
#'
#' @references McCrary, J. (2008).
#'   Manipulation of the running variable in the regression discontinuity design: A density test. 
#'   Journal of Econometrics, 142(2), 698-714. 
#'   \doi{10.1016/j.jeconom.2007.05.005}.
#'
#' @importFrom stats complete.cases sd lm coef predict pnorm setNames 
#' @importFrom graphics lines points axis box plot.new plot.window polygon title
#' @importFrom grDevices adjustcolor
#'
#' @include wt_kern.R
#'
#' @export
#'
#' @examples
#' # No discontinuity
#' x <- runif(1000, -1, 1)
#' dc_test(x, 0)
#' 
#' # Discontinuity
#' x <- runif(1000, -1, 1)
#' x <- x + 2 * (runif(1000, -1, 1) > 0 & x < 0)
#' dc_test(x, 0)

dc_test <- function(runvar, cutpoint, bin = NULL, bw = NULL, verbose = TRUE, plot = TRUE,
  ext.out = FALSE, htest = FALSE, level = 0.95, digits = max(3, getOption("digits") - 3),
  timeout = 30) {
  runvar.name = deparse(substitute(runvar))
  runvar <- runvar[complete.cases(runvar)]
  # Grab some summary vars
  rn <- length(runvar)
  rsd <- sd(runvar)
  rmin <- min(runvar)
  rmax <- max(runvar)
  if (missing(cutpoint)) {
    if (verbose) 
      cat("Assuming cutpoint of zero.\n")
      cat("\n")
    cutpoint <- 0
  }
  
  if (cutpoint <= rmin | cutpoint >= rmax) {
    stop("Cutpoint must lie within range of runvar.")
  }
  
  if (is.null(bin)) {
    bin <- 2 * rsd * rn^(-1/2)
  }
  if (verbose){
    cat("Binwidth:\n")
    cat(format(bin, digits = digits), "\n\n")    
  }

  l <- floor((rmin - cutpoint) / bin) * bin + bin / 2 + cutpoint  # Midpoint of lowest bin
  r <- floor((rmax - cutpoint) / bin) * bin + bin / 2 + cutpoint  # Midpoint of highest bin
  lc <- cutpoint - (bin / 2)  # Midpoint of bin just left of breakpoint
  rc <- cutpoint + (bin / 2)  # Midpoint of bin just right of breakpoint
  j <- floor((rmax - rmin) / bin) + 2
  
  binnum <- round((((floor((runvar - cutpoint)/bin) * bin + bin/2 + cutpoint) - l) / bin) + 1)
  
  # cellval stores the frequency/density of data in every binned cell
  fillcellval = function(j, rn, binnum){
    cellval <- tryCatch(rep(0, j), 
                        error = function(e){
                          e$message <- "Select a larger binwidth."
                          stop(e)
                        })
    
    for (i in seq(1, rn)) {
      cnum <- binnum[i]
      cellval[cnum] <- cellval[cnum] + 1
    }
    
    cellval <- (cellval / rn) / bin
    return(cellval)
  }
  cellval <- tryCatch(R.utils::withTimeout(fillcellval(j, rn, binnum), timeout = timeout),
                   TimeoutException = function(ex){
                     cat('Timeout. Select a larger binwidth or increase timeout argument.')
                     stop(ex)
                   })
  
  cellmp <- seq(from = 1, to = j, by = 1)
  cellmp <- floor(((l + (cellmp - 1) * bin) - cutpoint) / bin) * bin + bin / 2 + cutpoint
  
  # If no bandwidth is given, calc it
  if (is.null(bw)) {
    # bin number just left of breakpoint
    leftofc <- round((((floor((lc - cutpoint) / bin) * bin + bin / 2 + cutpoint) - l) / bin) + 1)
    # bin number just right of breakpoint
    rightofc <- round((((floor((rc - cutpoint) / bin) * bin + bin / 2 + cutpoint) - l) / bin) + 1)
    if (rightofc - leftofc != 1) {
      stop("Error occurred in bandwidth calculation.")
    }
    cellmpleft <- cellmp[1:leftofc]
    cellmpright <- cellmp[rightofc:j]
    
    # Estimate 4th order polynomial to the left
    P.lm <- tryCatch(R.utils::withTimeout(lm(cellval ~ poly(cellmp, degree = 4, raw = TRUE), 
                                   subset = cellmp < cutpoint), 
                                timeout = timeout),
                     TimeoutException = function(ex){
                       cat('Timeout. Select a larger binwidth or increase timeout argument.')
                       stop(ex)
                     })
    mse4 <- summary(P.lm)$sigma^2
    lcoef <- coef(P.lm)
    fppleft <- 2 * lcoef[3] + 6 * lcoef[4] * cellmpleft + 12 * lcoef[5] * cellmpleft * 
      cellmpleft
    hleft <- 3.348 * (mse4 * (cutpoint - l) / sum(fppleft * fppleft))^(1/5)
    
    # And to the right
    P.lm <- tryCatch(R.utils::withTimeout(lm(cellval ~ poly(cellmp, degree = 4, raw = TRUE), 
                                    subset = cellmp >= cutpoint), 
                                 timeout = timeout),
                     TimeoutException = function(ex){
                       cat('Timeout. Select a larger binwidth or increase timeout argument.')
                       stop(ex)
                     })
    mse4 <- summary(P.lm)$sigma^2
    rcoef <- coef(P.lm)
    fppright <- 2 * rcoef[3] + 6 * rcoef[4] * cellmpright + 12 * rcoef[5] * cellmpright * 
      cellmpright
    hright <- 3.348 * (mse4 * (r - cutpoint) / sum(fppright * fppright))^(1/5)
    
    bw <- 0.5 * (hleft + hright)
    if (is.nan(bw)){
      stop("Select a smaller binwidth.")
    }
    
  }
  if (verbose){
    cat("Bandwidth:\n")
    cat(format(bw, digits = digits), "\n\n")      
  } 
  
  if (sum(runvar > cutpoint - bw & runvar < cutpoint) == 0 | 
    sum(runvar < cutpoint + bw & runvar >= cutpoint) == 0) 
    stop("Insufficient data within the bandwidth.")
  if (plot) {
    # estimate density to either side of the cutpoint using a triangular kernel
    pmin <- cutpoint - 2 * rsd
    pmax <- cutpoint + 2 * rsd
    filld.l = function(cutpoint, bw, cellmp, cellval){
      d.l <- data.frame(cellmp = cellmp[cellmp < cutpoint], cellval = cellval[cellmp < cutpoint], 
                        dist = NA, est = NA, lwr = NA, upr = NA)
      for (i in 1:nrow(d.l)) {
        d.l$dist <- d.l$cellmp - d.l[i, "cellmp"]
        w <- wt_kern(d.l$dist, 0, bw, kernel = "triangular")
        newd <- data.frame(dist = 0)
        pred <- predict(lm(cellval ~ dist, weights = w, data = d.l), interval = "confidence", 
                        newdata = newd)
        d.l$est[i] <- pred[1]
        d.l$lwr[i] <- pred[2]
        d.l$upr[i] <- pred[3]
      }
      return(d.l)
    }
    d.l <- tryCatch(R.utils::withTimeout(filld.l(cutpoint, bw, cellmp, cellval), timeout = timeout),
                    TimeoutException = function(ex){
                      cat('Timeout. Select a larger binwidth or increase timeout argument.')
                      stop(ex)
                    })
    
    filld.r = function(cutpoint, bw, cellmp, cellval){
      d.r <- data.frame(cellmp = cellmp[cellmp >= cutpoint], cellval = cellval[cellmp >= cutpoint], 
                        dist = NA, est = NA, lwr = NA, upr = NA)
      for (i in 1:nrow(d.r)) {
        d.r$dist <- d.r$cellmp - d.r[i, "cellmp"]
        w <- wt_kern(d.r$dist, 0, bw, kernel = "triangular")
        newd <- data.frame(dist = 0)
        pred <- predict(lm(cellval ~ dist, weights = w, data = d.r), interval = "confidence", 
                        newdata = newd)
        d.r$est[i] <- pred[1]
        d.r$lwr[i] <- pred[2]
        d.r$upr[i] <- pred[3]
      }
      return(d.r)
    }
    d.r <- tryCatch(R.utils::withTimeout(filld.r(cutpoint, bw, cellmp, cellval), timeout = timeout),
                    TimeoutException = function(ex){
                      cat('Timeout. Select a larger binwidth or increase timeout argument.')
                      stop(ex)
                    })
    
    # plot to the left 

    # return(list(d.l,d.r))
    plot.new()
    ylim <- c(0,  # Wang: Modified
              max(c(cellval[cellmp <= pmax & cellmp >= pmin]), d.r$upr, d.l$upr))
    if (any(is.na(ylim))){
      stop("Error due to selected binwidth and bandwidth.")
    }
    plot.window(xlim = range(runvar, na.rm = TRUE), 
      ylim = ylim,
    )
    box()
    axis(1)
    axis(2)
    title(xlab = runvar.name, ylab = "Density Estimates")
    
    # Wang: Plot CIs as area (polygon)
    d.l2 <- rbind(setNames(d.l[order(d.l$cellmp), c("cellmp", "lwr")], c("x", "y")),
      setNames(d.l[order(d.l$cellmp, decreasing = TRUE), c("cellmp", "upr")], c("x", "y"))) 
    polygon(d.l2[,1], d.l2[,2], border = NA, col = adjustcolor("gray", alpha.f = .5))
    
    lines(d.l$cellmp, d.l$est, lty = 1, lwd = 2, col = "black", type = "l")
    
    # Wang: Not plot CIs as lines
    # lines(d.l$cellmp, d.l$lwr, lty = 2, lwd = 1, col = "black", type = "l")
    # lines(d.l$cellmp, d.l$upr, lty = 2, lwd = 1, col = "black", type = "l")
    
    # plot to the right
    
    # Wang: Plot CIs as area (polygon)
    d.r2 <- rbind(setNames(d.r[order(d.r$cellmp), c("cellmp", "lwr")], c("x", "y")),
      setNames(d.r[order(d.r$cellmp, decreasing = TRUE), c("cellmp", "upr")], c("x", "y"))) 
    polygon(d.r2[,1], d.r2[,2], border = NA, col = adjustcolor("gray", alpha.f = .5))
    
    lines(d.r$cellmp, d.r$est, lty = 1, lwd = 2, col = "black", type = "l")
    
    # Wang: Not plot CIs as lines
    # lines(d.r$cellmp, d.r$lwr, lty = 2, lwd = 1, col = "black", type = "l")
    # lines(d.r$cellmp, d.r$upr, lty = 2, lwd = 1, col = "black", type = "l")
    
    # plot the histogram as points
    points(cellmp, cellval, type = "p", pch = 20)

  }

  cmp <- cellmp
  cval <- cellval
  padzeros <- ceiling(bw / bin)
  jp <- j + 2 * padzeros
  if (padzeros >= 1) {
    cval <- c(rep(0, padzeros), cellval, rep(0, padzeros))
    cmp <- c(seq(l - padzeros * bin, l - bin, bin), cellmp, seq(r + bin, r + padzeros * bin, bin))
  }
  
  # Estimate to the left
  dist <- cmp - cutpoint
  w <- 1 - abs(dist / bw)
  w <- ifelse(w > 0, w * (cmp < cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatl <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  
  # Estimate to the right
  w <- 1 - abs(dist / bw)
  w <- ifelse(w > 0, w * (cmp >= cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatr <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  
  # Calculate and display dicontinuity estimate
  thetahat <- log(fhatr) - log(fhatl)
  sethetahat <- sqrt((1 / (rn * bw)) * (24/5) * ((1 / fhatr) + (1 / fhatl)))
  z <- thetahat / sethetahat
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  
  if (verbose) {
    cat("Estimate for log difference in heights:\n")
    stars <- if (is.na(p)) 
      " " else if (p < 0.001) 
        "***" else if (p < 0.01) 
          "**" else if (p < 0.05) 
            "*" else if (p < 0.1) 
              "." else " "
    alpha <- 1 - level
    lower.CL <- thetahat - qnorm(1-alpha/2) * sethetahat
    upper.CL <- thetahat + qnorm(1-alpha/2) * sethetahat
    out <- matrix(c(thetahat, sethetahat, lower.CL, upper.CL, z, p), nrow = 1)
    colnames(out) <- c("Estimate", "Std. Error", 
                       "lower.CL", "upper.CL",
                       "z value", "Pr(>|z|)") 
    rownames(out) <- ''
    #print.default(cbind(format(out, digits = digits), " " = stars), 
    #              quote = FALSE, print.gap = 2, right = FALSE)
    #cat("---\n")
    #cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
    #cat("Confidence interval used: ", level, "\n\n")
  }

  if (ext.out) 
    return(list(theta = thetahat, se = sethetahat, z = z, p = p, binsize = bin, 
      bw = bw, cutpoint = cutpoint, data = data.frame(cellmp, cellval))) 
  else if (htest) {
    # Return an htest object, for compatibility with base R test output.
    structure(list(statistic = c(z = z), p.value = p, method = "McCrary (2008) sorting test", 
      parameter = c(binwidth = bin, bandwidth = bw, cutpoint = cutpoint), 
      alternative = "no apparent sorting"), class = "htest")
  } else return(p)
}
