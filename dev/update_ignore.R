library(devtools)

# add a regular expression to .Rbuildignore
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_build_ignore("notes", escape = TRUE, pkg = "rddapp")
