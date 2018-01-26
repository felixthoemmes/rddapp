# set working directory
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

# find R script
system.file("R", "rd_est.R", package = "rddapp")

# find R Shiny
system.file("inst", "shinyrdd", package = "rddapp")

# find R dataset
system.file("data", "CARE", package = "rddapp")
