# install packages from CRAN
install.packages(c("devtools", "testthat", "roxygen2", "knitr", "rmarkdown"))
library(devtools)
library(testthat)
library(roxygen2)
library(knitr)
library(rmarkdown)

install.packages(c("rdd", "rdrobust", "rddtools", "foreign"))
library(rdd)
library(rdrobust)
library(rddtools)
library(foreign)

install.packages(c("AER", "sandwich", "lmtest", "Formula", "shiny", "DT"))
library(AER)
library(sandwich)
library(lmtest)
library(Formula)
library(shiny)
library(DT)

install.packages(c("stringi", "formatR", "lintr"))
library(stringi)
library(formatR)
library(lintr)

# install RStudio
install.packages("rstudioapi")
library(rstudioapi)
rstudioapi::isAvailable("0.99.149")

# install packages from github
# devtools::install_github("hadley/devtools")
# devtools::install_github("hadley/testthat")
# library(devtools)
# library(testthat)
devtools::has_devel()
devtools::session_info()

# see active libraries
.libPaths()
lapply(.libPaths(), dir)

# install any missing or outdated dependencies
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::install_deps("rddapp", dependencies = TRUE)
