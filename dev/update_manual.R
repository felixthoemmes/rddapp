# create the reference manual
if (.Platform$OS.type == "windows") {
  setwd("C:/Academia/Cornell/QML/RDD")
} else {
  setwd("~")
}

system("R CMD Rd2pdf rddapp")
system("mv rddapp.pdf rddapp/dev/doc")
# system("R CMD check rddapp")
