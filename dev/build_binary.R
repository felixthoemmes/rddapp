library(devtools)

# build a binary package in a platform specific form from the source package
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::build("rddapp", binary = TRUE, manual = TRUE)
system("mv *.tar.gz rddapp/dev/doc")
