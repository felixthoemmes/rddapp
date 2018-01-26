library(devtools)

# include the code used to update or reproduce the data
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_data_raw("rddapp")
