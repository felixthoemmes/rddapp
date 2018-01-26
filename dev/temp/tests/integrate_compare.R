#
library(kulife)
library(MESS)
library(caTools)

# Trapezoidal rule
# https://en.wikipedia.org/wiki/Trapezoidal_rule

trapez <- function(x, y) {
  if (length(x) != length(y)) {
    stop('dimensions of x and y do not agree')
  }
  
  n <- length(x)
  
  sum(diff(x) * (y[2:n] + y[1:(n - 1)])) / 2
}

#
set.seed(12345)

#
x <- seq(0, 10)
y <- rep(1, 11)
trapez(x, y) == 10
kulife::auc(x, y) == 10
MESS::auc(x, y) == 10
caTools::trapz(x, y) == 10

#
x <- seq(0, 10)
y <- seq(0, 10)
trapez(x, y) == 50
kulife::auc(x, y) == 50
MESS::auc(x, y) == 50
caTools::trapz(x, y) == 50

#
x <- seq(0, 100)
y <- rnorm(101)
trapez(x, y)
kulife::auc(x, y)
MESS::auc(x, y)
caTools::trapz(x, y)

#
x <- c(rep(-1, 100), seq(0, 100))
y <- c(rep(1, 100), rnorm(101))
trapez(x, y)
kulife::auc(x, y)
MESS::auc(x, y)
caTools::trapz(x, y)

#
x <- seq(0, 100)
y <- rnorm(101)
x <- rep(x, 2)
y <- rep(y, 2)
y <- y[order(x)]
x <- x[order(x)]
trapez(x, y)
kulife::auc(x, y)
MESS::auc(x, y)
caTools::trapz(x, y)

# 
x <- rnorm(100)
y <- rnorm(100)
kulife::auc(x, y)
MESS::auc(x, y)
y <- y[order(x)]
x <- x[order(x)]
trapez(x, y)
caTools::trapz(x, y)
