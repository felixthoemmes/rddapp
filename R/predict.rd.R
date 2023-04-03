#' Predict the Regression Discontinuity
#' 
#' \code{predict.rd} makes predictions of means and standard deviations of RDs at different cutoffs.
#' 
#' @param object An \code{rd} object, typically the result of \code{\link{rd_est}}.
#' @param gran A non-negative integer specifying the granularity of the data points (i.e. the desired number of predicted points). The default is 50.
#' @param ... Additional arguments passed to \code{predict}.
#'
#' @importFrom AER ivreg 
#' @importFrom Formula as.Formula
#' @importFrom stats reshape vcov
#'
#' @include rd_est.R
#' @include treat_assign.R
#'
#' @export
#'
#' @examples 
#' set.seed(12345)
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' tr <- as.integer(x >= 0)
#' rd <- rd_est(y ~ x + tr | cov, cutpoint = 0, t.design = "geq") 
#' predict(rd)

predict.rd <- function(object, gran = 50, ...) {
  if (!inherits(object, "rd"))
    stop("Not an object of class rd.")

  if ("kernel" %in% names(object$call)) 
    kern <- eval.parent(object$call$kernel) 
  else kern <- "triangular"
  
  if ("cutpoint" %in% names(object$call)) 
    cut <- eval.parent(object$call$cutpoint) 
  else cut <- 0
  
  if ("t.design" %in% names(object$call))
    t.design <- eval.parent(object$call$t.design) 
  else t.design <- "l"
  
  if ("est.itt" %in% names(object$call))
    est.itt <- eval.parent(object$call$est.itt) 
  else est.itt <- FALSE

  x_range <- range(object$frame$X)

  # DETERMINE IF Z EXISTS 
  is.fuzzy <- FALSE
  if ("Z" %in% names(object$frame)) {
    models <- object$model$iv
    if (!est.itt)
      is.fuzzy <- TRUE
  } else {
    models <- object$model
  }
  
  # reverse_tr = cor(t_star, object$frame$Z) < 0
  # if (is.na(reverse_tr))

  #### PARAMETRIC PREDICTIONS ####   
  
  # PUT MORE POINTS FOR SMOOTHIER PREDICTED LINES
  df_p <- data.frame(X = seq(min(object$frame$X), max(object$frame$X), length.out = gran))

  df_p <- within(df_p, {
    cf <- 0  # this indicate if the point is counterfacual (or observed)
    Tr <- treat_assign(df_p$X, cut, t.design)
    X1 <- df_p$X - cut
    X2 <- X1^2 
    X3 <- X1^3 
  })
  
  # ADD COUNTERFACTUAL POINTS AT THE CUTOFF 
  df_p_cut <- if (cut %in% df_p$X) {
    within(subset(df_p, df_p$X == cut), {
      Tr <- if (grepl("eq", t.design)) 0 else 1
      cf <- 1
    })
  } else {
    data.frame(
      X = rep(cut, 2),
      X3 = c(0, 0),  
      X2 = c(0, 0),  
      X1 = c(0, 0), 
      Tr = if (grepl("eq", t.design)) c(0, 1) else c(1, 0), 
      cf = c(1, 0))
  }
  
  df_p <- rbind(df_p, setNames(df_p_cut, names(df_p)))
  
  # ADD COVS AT GRAND MEAN 
  if (!is.null(object$cov)) {
    covs_gm <- paste0(object$cov, "_gm")
    df_p <- cbind(df_p, 
      setNames(lapply(subset(object$frame, select = object$cov), mean, na.rm = TRUE), nm = covs_gm))
  } else {
    covs_gm <- c()
  }

  # PREDICTION BASED ON STAGE 2
  df_p <- within(df_p, {
    # PREDICTED MEANS (COVS AT GRAND MEANS)
    Yhat.linear <- predict(models[[1]], 
      newdata = setNames(data.frame(Tr, Tr, X1 * (1 - Tr), X1 * Tr, df_p[covs_gm]), 
        c("Tr", "Z", "Xl", "Xr", object$cov))
    )
    
    Yhat.quadratic <- predict(models[[2]], 
      newdata = setNames(data.frame(Tr, Tr, X1 * (1 - Tr), X1 * Tr, df_p[covs_gm]),
        c("Tr", "Z", "Xl", "Xr", object$cov))
    )
    
    Yhat.cubic <- predict(models[[3]], 
      newdata = setNames(data.frame(Tr, Tr, X1 * (1 - Tr), X1 * Tr, df_p[covs_gm]),
        c("Tr", "Z", "Xl", "Xr", object$cov))
    )
    
    # PREDICTED VARIANCES (COVS AT MEANS AT EACH X)
    YSE.linear <- pred_var(
      as.matrix(cbind(1, Tr, X1 * (1 - Tr), X1 * Tr, df_p[covs_gm])),
      vcov(models[[1]]))
    
    YSE.quadratic <- pred_var(
      as.matrix(cbind(1, Tr, X1 * (1 - Tr), X2 * (1 - Tr), X1 * Tr, X2 * Tr, df_p[covs_gm])),
      vcov(models[[2]]))
    
    YSE.cubic <- pred_var(
      as.matrix(cbind(1, Tr, X1 * (1 - Tr), X2 * (1 - Tr), X3 * (1 - Tr), X1 * Tr, X2 * Tr, 
        X3 * Tr, df_p[covs_gm])),
      vcov(models[[3]]))
  })
  
  df_p <- df_p[order(df_p$X),]
  
  #### NONPARAMETRIC PREDICTIONS ####
  df <- within(object$frame, {
    Tr <- treat_assign(object$frame$X, cut, t.design)
    X1 <- object$frame$X - cut
    X2 <- X1^2 
    X3 <- X1^3 
  })
  
  # NON-CUTOFF POINTS (REFITTING)
  df_np <- do.call(rbind.data.frame, 
    # repeat for each unique(X), except for cut point 
    lapply(setdiff(seq(min(object$frame$X), max(object$frame$X), length.out = gran), cut), 
      FUN = function(Xi) {
        # repeat for each bw
        result <- lapply(object$bw[4:6], 
          FUN = function(bw) {
            w <- wt_kern(X = df$X, center = Xi, bw = bw, kernel = kern)
            m <- try({
              if (is.fuzzy)
                ivreg(as.Formula(
                  sprintf(
                    "Y ~ Z + I(X1 * (1-Tr)) + I(X1 * Tr) %s | Tr + I(X1 * (1-Tr)) + I(X1 * Tr) %s ", 
                    paste(c("", object$cov), collapse = " + "), 
                    paste(c("", object$cov), collapse = " + "))),
                  data = df, weights = w)
              else
                lm(as.formula(
                  sprintf("Y ~ Tr + I(X1 * (1-Tr)) + I(X1 * Tr) %s", 
                    paste(c("", object$cov), collapse = " + "))),
                  data = df, weights = w)
            })

            if (inherits(m, "try-error")) {
              return(c(Yhat = NA, YSE = NA))
            } else {
              if (any(is.na(coef(m)))) 
                return(c(Yhat = NA, YSE = NA))
            }

            # Stage 2 
            Tr <- treat_assign(Xi, cut, t.design)
            
            X_covs_gm <- c(1, 
              Z = Tr, 
              Xl = (Xi - cut) * (1 - Tr), 
              Xr = (Xi - cut) * Tr, 
              unlist(sapply(object$frame[object$cov], mean, na.rm = TRUE)))
            
            return(c(
              Yhat = X_covs_gm %*% coef(m), 
              YSE = pred_var(X_covs_gm, vcov(m))  # COVS AT GRAND MEAN
            ))
          }
        )

        return(data.frame(do.call(rbind, result), X = Xi, bw = c("optimal", "half", "double")))       
      }
    )
  )

  df_np$cf <- 0
  
  # CUTOFF POINTS (NO REFITTING)
  cf <- if (grepl("eq", t.design)) 1:0 else 0:1
  
  # Stage 2 
  X_covs_gm <- rbind(
    c(1, Z_hat = 0, 0, 0, unlist(sapply(object$frame[object$cov], mean, na.rm = TRUE))),
    c(1, Z_hat = 1, 0, 0, unlist(sapply(object$frame[object$cov], mean, na.rm = TRUE)))
  )
  
  df_np <- rbind.data.frame(df_np, 
    data.frame(
      Yhat = X_covs_gm %*% coef(models[[4]]), 
      YSE = pred_var(X_covs_gm, vcov(models[[4]])), 
      X = cut, bw = "optimal", cf = cf), 
    data.frame(
      Yhat = X_covs_gm %*% coef(models[[5]]), 
      YSE = pred_var(X_covs_gm, vcov(models[[5]])), 
      X = cut, bw = "half", cf = cf), 
    data.frame(
      Yhat = X_covs_gm %*% coef(models[[6]]), 
      YSE = pred_var(X_covs_gm, vcov(models[[6]])),
      X = cut, bw = "double", cf = cf)
  )
  
  df_np <- reshape(df_np, direction = "wide", v.names = c("Yhat", "YSE"), idvar = c("X", "cf"), 
    timevar = "bw")
  
  pred <- merge(df_p, df_np, by = c("X", "cf"), all.x = TRUE)
  pred <- pred[order(pred$X), ]
  
  return(pred)
}

pred_var <- function(data_mat, vcov) {
  vars <- try(sqrt(rowSums(data_mat %*% vcov * data_mat)))
  if (inherits(vars, "try-error"))
    vars <- NA
  return(vars)
}

