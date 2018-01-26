library(devtools)

# creare a vignette
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_vignette("rddapp", pkg = "rddapp")
