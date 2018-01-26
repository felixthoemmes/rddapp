library(knitr)

# knit the vignette in a specific format
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

setwd("rddapp/dev/doc")
knit("../../vignettes/rddapp.Rmd")
# knit("../../vignettes/rddapp.Rnw") 
# knit("../../vignettes/rddapp.Rtex")
# knit("../../vignettes/rddapp.Rhtml")
