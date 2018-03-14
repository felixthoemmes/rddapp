## Convert .Rmd to .html readible by shiny

library("rmarkdown")
filename <- "help_page"
render(input=paste0(filename,".Rmd"),
       output_file=paste0("../www/",filename,".html"))
