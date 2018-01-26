library(devtools)

# install a package from the source package
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::install("rddapp", build_vignettes = TRUE)
