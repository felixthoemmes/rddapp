library(devtools)

# build a bundled package in .tar.gz from the source package
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::build("rddapp", manual = TRUE)
system("mv *.tar.gz rddapp/dev/doc")
