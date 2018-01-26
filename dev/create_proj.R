library(devtools)

# create an .Rproj file for an existing package
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_rstudio("rddapp")
