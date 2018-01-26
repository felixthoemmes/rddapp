library(devtools)

# set up a basic .travis.yml config file
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_travis("rddapp")
