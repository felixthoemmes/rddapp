constant <- function() {
  integrand <- function(dist) {
    basis(dist)
  }
  v0 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)*dist
  }
  v1 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)*(dist^2)
  }
  v2 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)*(dist^3)
  }
  v3 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)^2
  }
  p0 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)^2*dist
  }
  p1 <- integrate(integrand, lower = 0, upper = 1)$value
  integrand <- function(dist) {
    basis(dist)^2*(dist^2)
  }
  p2 <- integrate(integrand, lower = 0, upper = 1)$value
  
  c1 <- ((v2^2 - v1 * v3)/(v2 * v0 - v1^2))^2/4
  c2 <- (v2^2 * p0 - 2 * v1 * v2 * p1 + v1^2 * p2)/((v2 * v0 - v1^2)^2)
  
  (c2/c1/4)^(1/5)
} 

#
basis <- function(dist) {
  1-abs(dist) 
}
constant()

#
basis <- function(dist) {
  1/2
}
integrand <- function(dist) {
  basis(dist)
}
v0 <- 1/2
integrand <- function(dist) {
  basis(dist)*dist
}
v1 <- integrate(integrand, lower = 0, upper = 1)$value
integrand <- function(dist) {
  basis(dist)*(dist^2)
}
v2 <- integrate(integrand, lower = 0, upper = 1)$value
integrand <- function(dist) {
  basis(dist)*(dist^3)
}
v3 <- integrate(integrand, lower = 0, upper = 1)$value
integrand <- function(dist) {
  basis(dist)^2
}
p0 <- 1/4
integrand <- function(dist) {
  basis(dist)^2*dist
}
p1 <- integrate(integrand, lower = 0, upper = 1)$value
integrand <- function(dist) {
  basis(dist)^2*(dist^2)
}
p2 <- integrate(integrand, lower = 0, upper = 1)$value

c1 <- ((v2^2 - v1 * v3)/(v2 * v0 - v1^2))^2/4
c2 <- (v2^2 * p0 - 2 * v1 * v2 * p1 + v1^2 * p2)/((v2 * v0 - v1^2)^2)

(c2/c1/4)^(1/5)

#
basis <- function(dist) {
  3/4*(1-dist^2)
}
constant()

#
basis <- function(dist) {
  15/16*(1-dist^2)^2
}
constant()

#
basis <- function(dist) {
  35/32*(1-dist^2)^3
}
constant()

#
basis <- function(dist) {
  70/81*(1-abs(dist)^3)^3
}
constant()

#
basis <- function(dist) {
  1/sqrt(2*pi)*exp(-1/2*dist^2)
}
integrand <- function(dist) {
  basis(dist)
}
v0 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)*dist
}
v1 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)*(dist^2)
}
v2 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)*(dist^3)
}
v3 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)^2
}
p0 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)^2*dist
}
p1 <- integrate(integrand, lower = 0, upper = Inf)$value
integrand <- function(dist) {
  basis(dist)^2*(dist^2)
}
p2 <- integrate(integrand, lower = 0, upper = Inf)$value

c1 <- ((v2^2 - v1 * v3)/(v2 * v0 - v1^2))^2/4
c2 <- (v2^2 * p0 - 2 * v1 * v2 * p1 + v1^2 * p2)/((v2 * v0 - v1^2)^2)

(c2/c1/4)^(1/5)

#
basis <- function(dist) {
  pi/4*cos(pi/2 * dist)
}
constant()

if(kernel=="triangular"){
  w<-(1-abs(dist))
} else if (kernel=="rectangular") {
  w<-1/2
} else if (kernel=="epanechnikov") {
  w<-3/4*(1-dist^2)
} else if (kernel=="quartic" | kernel=="biweight") {
  w<-15/16*(1-dist^2)^2
} else if (kernel=="triweight") {
  w<-35/32*(1-dist^2)^3
} else if (kernel=="tricube") {
  w<-70/81*(1-abs(dist)^3)^3
} else if (kernel=="gaussian") {
  w<-1/sqrt(2*pi)*exp(-1/2*dist^2)
} else if (kernel=="cosine") {
  w<-pi/4*cos(pi/2 * dist)