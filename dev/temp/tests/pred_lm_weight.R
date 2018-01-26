set.seed(12345)
y <- rnorm(100)
x <- rnorm(100)
z <- rnorm(100)
w <- seq(0, 1, length.out = 100)

# lm
mod1 <- lm(y ~ x + z)
summary(mod1)
new <- data.frame(x = 1, z = 1)
new_pred <- predict.lm(mod1, new, interval = 'confidence')
new_mean <- c(1, as.numeric(new)) %*% mod1$coefficients
new_var <- sum(c(1, as.numeric(new)) %*% vcov(mod1) * c(1, as.numeric(new)))
new_upper <- new_mean + qt(0.975, mod1$df.residual) * sqrt(new_var)
new_upper == new_pred[3]

# weighted lm
mod2 <- lm(y ~ x + z, weights = w)
summary(mod2)
new <- data.frame(x = 1, z = 1)
new_pred <- predict.lm(mod2, new, interval = 'confidence')
new_mean <- c(1, as.numeric(new)) %*% mod2$coefficients
new_var <- sum(c(1, as.numeric(new)) %*% vcov(mod2) * c(1, as.numeric(new)))
new_upper <- new_mean + qt(0.975, mod2$df.residual) * sqrt(new_var)
new_upper == new_pred[3]
