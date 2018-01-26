library(devtools)

# generate documents in .Rd from comments
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::document("rddapp")
