#
den_kern <- function(x, newx, bw, fun) {
  n <- length(x)
  newn <- length(newx)
  
  colMeans(fun((matrix(rep(newx, n), n, newn, byrow = T) - x) / bw)) / bw
}

#
x <- rnorm(10)
newx <- rnorm(3)
bw <- 1
fun <- dnorm

den_kern(x, newx, bw, fun)

#
x <- sort(rnorm(1000))
newx <- x
bw <- 1
fun <- dnorm

d1 <- den_kern(x, newx, bw, fun)
d2 <- density(x, bw = bw, kernel = 'gaussian')

plot(newx, d1, col = 'blue', type = 'l')
lines(d2$x, d2$y, col = 'red')


