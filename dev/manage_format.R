# clean up format
library(formatR)

if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

formatR::tidy_dir("rddapp", indent = 2, width.cutoff = 80, comment = TRUE, arrow = TRUE)

# warn about format
library(lintr)
lintr::lint_package()
