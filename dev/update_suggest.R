library(devtools)

# add a package to suggests
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

args <- commandArgs(TRUE)
devtools::use_package(args[1], type = "Suggests", pkg = "rddapp")
