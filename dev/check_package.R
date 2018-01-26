library(devtools)

# check the source package and record the time
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

system.time(devtools::check("rddapp", manual = TRUE))
