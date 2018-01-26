#
set.seed(12345)

#
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)

mod <- rd_est(y ~ x, cutpoint = 0, model = TRUE)$model
newx <- runif(10, -1, 1)

for (i in 1:6) {
  predict(mod[[i]], newdata = 
            data.frame(Tr = as.integer(newx >= 0), 
                       Xl = (newx < 0) * newx, Xr = (newx >= 0) * newx))
}

#
x <- rnorm(100)
y <- rnorm(100)
newx <- rnorm(10)

mod1 <- lm(y ~ x)
predict(mod1)
predict(mod1, newdata = data.frame(x = newx))

mod2 <- lm(y ~ poly(x, degree = 3, raw = T))
predict(mod2)
predict(mod2, newdata = data.frame(x = newx))

mod3 <- lm(as.formula('y ~ x'))
predict(mod3)
predict(mod3, newdata = data.frame(x = newx))

mod4 <- lm(as.formula('y ~ poly(x, degree = 3, raw = T)'))
predict(mod4)
predict(mod4, newdata = data.frame(x = newx))

form5 <- as.formula('y ~ x')
mod5 <- lm(form5)
predict(mod5)
predict(mod5, newdata = data.frame(x = newx))

form6 <- as.formula('y ~ poly(x, degree = 3, raw = T)')
mod6 <- lm(form6)
predict(mod6)
predict(mod6, newdata = data.frame(x = newx))



