#' Plot the Multivariate Frontier Regression Discontinuity
#'
#' \code{plot.mfrd} plots the 3D illustration of the bivariate frontier RDD.
#'
#' @param x An \code{mfrd} object returned by \code{mfrd_est} or contained in the object 
#'   returned by \code{mrd_est}.
#' @param model Option for the model specification, one of \code{c("m_s", "m_h", "m_t")},
#'   which denotes the complete model, heterogeneous treatment model and treatment only 
#'   model respectively.
#' @param gran Granularity of the surface grid i.e. desired number of predicted points 
#'   before and after the cutoff along each assignment variable.
#' @param raw_data Whether the raw data points are plotted.
#' @param color_surface Whether the treated surface is colored.
#' @param ... Additional graphic arguments passed to \code{persp}.
#'
#' @importFrom stats expand.model.frame formula
#' @importFrom graphics persp 
#' @importFrom grDevices trans3d
#'
#' @include mfrd_est.R
#'
#' @export
#'
#' @examples
#' set.seed(12345)
#' x1 <- runif(1000, -1, 1)
#' x2 <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * (x1 >= 0) + 3 * cov + 10 * (x2 >= 0) + rnorm(1000)
#' model <- mfrd_est(y = y, x1 = x1, x2 = x2, c1 = 0, c2 = 0, t.design = c("geq", "geq"))
#' plot(model, "m_s")

plot.mfrd <- function(x, model = c("m_s", "m_h", "m_t"), gran = 2, raw_data = TRUE, 
  color_surface = FALSE, ...) {
  
  if (class(x)!= "mfrd") 
    stop ("Not an object of class mfrd.")
  
  model <- match.arg(model)
  
  m <- x[[model]]
  
  c1 <- eval.parent(x$call$c1)
  c2 <- eval.parent(x$call$c2)
  
  if ("t.design" %in% names(x$call))
    t.design <- eval.parent(x$call$t.design) 
  else t.design <- c("l", "l")
  
  if ("local" %in% names(x$call))
    local <- eval.parent(x$call$local) 
  else local <- 0.15
  
  if (is.null(m$call$data)) 
    m$call$data <- environment(formula(m))
  
  frame <- expand.model.frame(m, ~ x1 + x2)
  names(frame)[2:3] <- c("zcx1", "zcx2")
  
  frame$x1res <- as.integer(frame$zcx1 <= local & frame$zcx1 >= -local)
  frame$x2res <- as.integer(frame$zcx2 <= local & frame$zcx2 >= -local)
  
  frame$is_local <- frame$x1res | frame$x2res 
  
  # generate data for plotting
  ratio1 <- (c1 - min(frame$x1)) / (max(frame$x1) - min(frame$x1))
  ratio2 <- (c2 - min(frame$x2)) / (max(frame$x2) - min(frame$x2))
  
  newdata <- expand.grid(
    x1 = c(seq(min(frame$x1), c1 - 1e-10, length.out = ifelse(gran > 4, round(gran * ratio1), 2)),
      seq(c1 + 1e-10, max(frame$x1), length.out = ifelse(gran > 4, round(gran * (1 - ratio1)), 2))), 
    x2 = c(seq(min(frame$x2), c2 - 1e-10, length.out = ifelse(gran > 4, round(gran * ratio2), 2)),
      seq(c2 + 1e-10, max(frame$x2), length.out = ifelse(gran > 4, round(gran * (1 - ratio2)), 2)))
  )
  
  newdata$tr1 <- treat_assign(newdata$x1, c1, t.design[1])
  newdata$tr2 <- treat_assign(newdata$x2, c2, t.design[2])
  newdata$tr <- as.integer(newdata$tr1 | newdata$tr2)
  
  # newdata$cov = mean(frame$cov)  # not implemented yet
  
  newdata$yhat <- predict(m, newdata = newdata)
  
  newdata$quandrant <- interaction(newdata$tr1, newdata$tr2)
  
  newdata <- merge(newdata, 
    data.frame(
      quandrant = c("0.0", "0.1", "1.0", "1.1"),
      color = c(NA, "#428bca", "#428bca", "#428bca")
    )
  )
  
  newdata <- newdata[order(newdata$x1, newdata$x2, newdata$tr1, newdata$tr2), ]
  
  # plotting  
  preds <- list(
    x1 = sort(c(unique(newdata$x1))),
    x2 = sort(c(unique(newdata$x2))),
    yhat = matrix(newdata$yhat, ncol = sqrt(nrow(newdata)), byrow = TRUE)
  )
  
  ele_3d <- persp(preds$x1, preds$x2, preds$yhat, 
    xlim = range(c(frame$x1, newdata$x1)),
    ylim = range(c(frame$x2, newdata$x2)),
    zlim = range(c(frame$y, newdata$y)), ...)
  
  # color panels
  if (color_surface) {
    by(newdata, newdata$quandrant, 
      function(frame) {
        frame <- subset(frame, frame$x1 %in% range(frame$x1) & frame$x2 %in% range(frame$x2))
        poly_3d <- as.data.frame(trans3d(frame$x1, frame$x2, frame$yhat, pmat = ele_3d))
        poly_3d <- poly_3d[c(1, 2, 4, 3), ]
        polygon(poly_3d$x, poly_3d$y, col = adjustcolor(frame$color, alpha.f = .2), border = NA)
      }
    )
  }
  
  if (raw_data) {
    pts_3d <- trans3d(frame$x1, frame$x2, frame$y, pmat = ele_3d)
    points(pts_3d, pch = ifelse(frame$tr, 19, 1), cex = .7)
  }

  # if (local_data) {
  #   # revert dat_h$h1 dat_h$h2 back to the original scale of x1 and x2
  #   mx1 <- mean(frame$x1)
  #   mx2 <- mean(frame$x2)
  #   sdx1 <- sd(frame$x1)
  #   sdx2 <- sd(frame$x2)
  #   zc1 <- scale(c1, scale = sdx1, center = mx1)
  #   zc2 <- scale(c2, scale = sdx2, center = mx2)
    
  #   newdata_h <- subset(x$dat_h, select = c("h1", "h2", "tr", "tr1", "tr2"))
  #   newdata_h$x1 <- (x$dat_h$h1 + zc1) * sdx1 + mx1
  #   newdata_h$x2 <- (x$dat_h$h2 + zc2) * sdx2 + mx2
    
  #   newdata_h$gx1 <- m$coefficients["tr"] * newdata_h$tr + m$coefficients["tr2"] * newdata_h$tr2 + 
  #     m$coefficients[grep("x1(?!.*x2).*:tr2", names(m$coefficients), perl = TRUE)] * newdata_h$tr2 * 
  #     (newdata_h$x1 - zc1)
    
  #   newdata_h$gx2 <- m$coefficients["tr"] * newdata_h$tr + m$coefficients["tr1"] * newdata_h$tr1 + 
  #     m$coefficients[grep("x2(?!.*x1).*:tr1", names(m$coefficients), perl = TRUE)] * newdata_h$tr1 * 
  #     (newdata_h$x2 - zc2)

  #   h1_3d <- with(newdata_h, trans3d(x1,c2, gx1, pmat = ele_3d))
  #   h2_3d <- with(newdata_h, trans3d(c1, x2, gx2, pmat = ele_3d))
  #   points(h1_3d, pch = 19,  # cex = .7,
  #     col = ifelse(x$dat_h$tr2, "blue", "red") , cex = x$dat_h$fx1)
  #   points(h2_3d, pch = ifelse(x$dat_h$tr, 19, 1),  # cex = .7,
  #     col =ifelse(x$dat_h$tr1, "green", "brown"), cex=x$dat_h$fx2)
  # }
}
