#' Plot the Simulated Estimates for Sensitivity Analyses
#'
#' \code{sens_plot} plots the sensitivity analysis for cutpoints or bandwidths.
#'
#' @param sim_results A \code{data.frame} returned by \code{rd_sens_cutoff}, \code{rd_sens_bw},
#'   \code{mrd_sens_cutoff}, or \code{mrd_sens_bw}.
#' @param level A numeric value between 0 and 1 specifying the confidence level for CIs (assuming a normal sampling distribution). The default is 0.95.
#' @param x A string of the column name of the varying parameter in \code{sim_results}. 
#'   This will be used as the x-axis in the plot. Possible values are \code{c("A1", "A2", "bw")}, 
#'   which are column names in \code{sim_results}. 
#'   \code{A1} specifies that the varying cutoffs are for assignment 1 and \code{A2} specifies assignment 2.
#'   \code{bw} indicates that the varying parameter is bandwidth.  
#' @param plot_models A character vector specifying the models to be plotted (i.e. models estimated with 
#'   different approaches). Possible values are \code{unique(sim_results$model))}.
#' @param yrange An optional numeric vector specifying the range of the y-axis.
#'
#' @importFrom stats na.omit
#' @importFrom graphics abline
#'
#' @export
#'
#' @examples
#' set.seed(12345)
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' m <- rd_est(y ~ x | cov, t.design = "geq")
#' sim_cutoff <- rd_sens_cutoff(m, seq(-.5, .5, length.out = 10))
#' sens_plot(sim_cutoff, x = "A1", plot_models = c("linear", "optimal"))
#' sim_bw <- rd_sens_bw(m, seq(.1, 1, length.out = 10))
#' sens_plot(sim_bw, x = "bw")

sens_plot <- function(sim_results, level = .95, x = c("A1", "A2", "bw"),
  plot_models = unique(sim_results$model), yrange = NULL) { 
  plot_data <- subset(sim_results, sim_results$model %in% plot_models)
  plot_data$lwr <- plot_data$est + qnorm((1 - level) / 2) * plot_data$se
  plot_data$upr <- plot_data$est + qnorm((1 - level) / 2, lower.tail = FALSE) * plot_data$se
  
  plot_data <- plot_data[order(plot_data[, x]), ]

  plot_data$type <- sub("(center|univ[12])-", "", plot_data$model)
  
  if (is.null(yrange)) {
    if (cumprod(range(plot_data$est, na.rm = TRUE))[2] < 0) {
      yrange <- range(plot_data$est, na.rm = TRUE)
    } else {
      yrange <- range(plot_data$est, 0, na.rm = TRUE)  
    }
  }
  
  ## add buffer space in y limits for legends
  dy <- 0.3
  yrange <- c((1-dy)*yrange[1], (1+dy)*yrange[2])

  approach <- regmatches(plot_data$model, regexpr("(center|univ[12])", plot_data$model)) 
  plot_data$approach <- if (length(approach) == 0) "regular" else approach
  plot_data$color <- sapply(plot_data$type, switch, 
    linear = "black", quadratic = "red4", cubic = "blue4", 
    optimal = "red", half = "blue", double = "green4", "black")
  plot_data$shape <- sapply(plot_data$type, switch, 
    linear = 0, quadratic = 1, cubic = 2, optimal = 15, half = 16, double = 17, 0)
  
  plot_data$lty <- if (x != "bw") 
    sapply(plot_data$approach, switch, center = 1, univ1 = 2, univ2 = 3, 1) 
    else 1
  
  plot_data$x <- plot_data[, x]
  if (x == "bw") {
    plot_data$model <- "bw"
  }
  plot.new()
  plot.window(xlim = range(plot_data$x), 
    ylim = yrange + ifelse(x != "bw", c(0, .25 * (yrange[2] - yrange[1])), 0))
  
  by(plot_data, plot_data$model, 
    function(df) {
      poly_coordinate <- rbind(setNames(df[order(df$x), c("x", "lwr")], c("x", "ci")),
        setNames(df[order(df$x, decreasing = TRUE), c("x", "upr")],c("x", "ci")))
      
      poly_coordinate <- as.matrix(na.omit(poly_coordinate))
      
      polygon(poly_coordinate, col = adjustcolor(df$color, alpha.f = .1), border = NA)
      
      points(est ~ x, data= df, pch = df$shape, lwd = 2, col = df$color, type = "b", lty = df$lty)
    }
  )

  # Plot Legend
  pos0 <- legend(x = "topleft", legend = "", bty = "n")
  
  para_legend <- unique(subset(plot_data, plot_data$type %in% c("linear", "quadratic", "cubic"),
      select = c("model", "approach", "type", "color", "shape", "lty")))
  np_legend <- unique(subset(plot_data, plot_data$type %in% c("optimal", "half", "double"),
      select = c("model", "approach", "type", "color", "shape", "lty")))

  if (nrow(para_legend) > 0) {  
    pos1 <- legend(
      x = pos0$rect$left + pos0$rect$w + strwidth("parametric"), y = pos0$rect$top, 
      legend = rep("", nrow(para_legend) + 1), col = c(NA, para_legend$color), 
      pch = c(NA, para_legend$shape), lty = c(NA, para_legend$lty),
      lwd = 2, bty = "n", merge = TRUE)
    
    text(pos1$rect$left, pos1$text$y, adj = 1, labels = c("parametric:", para_legend$model))
  } else {
    pos1 <- pos0
  }
  
  if (nrow(np_legend) > 0) {
    pos2 <- legend(
      x = pos1$rect$left + pos1$rect$w + strwidth("nonparametric"), y = pos0$rect$top, 
      legend = rep("", nrow(np_legend) + 1), col = c(NA, np_legend$color),
      pch = c(NA, np_legend$shape), lty = c(NA, np_legend$lty),
      lwd = 2, bty = "n", merge = TRUE)
    
    text(pos2$rect$left, pos2$text$y, adj = 1, labels = c("nonparametric:", np_legend$model))
  }
  
  # Finish Ploting
  box()
  axis(1)
  axis(2)
  abline(h = 0, lty = 2)
}
