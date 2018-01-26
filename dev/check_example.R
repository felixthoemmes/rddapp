library(devtools)

# check the examples and record the time
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

system.time(devtools::run_examples("rddapp"))
