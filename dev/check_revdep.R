library(devtools)

# check the downstream dependencies
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::revdep_check("rddapp")