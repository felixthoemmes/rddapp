library(devtools)

# load data
if (.Platform$OS.type == "windows") {
  setwd("C:/Users/Ze/Desktop")
} else {
  setwd("~")	
}

f1cov <- read.csv("f1cov.csv")
f1nocov <- read.csv("f1nocov.csv")
f2cov <- read.csv("f2cov.csv")
f2nocov <- read.csv("f2nocov.csv")
s1cov <- read.csv("s1cov.csv")
s1nocov <- read.csv("s1nocov.csv")
s2cov <- read.csv("s2cov.csv")
s2nocov <- read.csv("s2nocov.csv")

CARE <- read.csv("dat_s.csv")

# update a .RData file created by save() containing a single object
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

devtools::use_data(f1cov, f1nocov, f2cov, f2nocov, s1cov, s1nocov, s2cov, s2nocov, 
  pkg = "rddapp", overwrite = TRUE)

devtools::use_data(CARE, pkg = "rddapp", overwrite = TRUE)
