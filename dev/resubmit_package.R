library(devtools)

# release the source package and record the time
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

submit_cran("rddapp")
