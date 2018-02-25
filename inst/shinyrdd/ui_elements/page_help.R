## Attempt 1: no TOC
##includeMarkdown("help_page_content/help_page_full.Rmd")

## Attempt 2: works if we only include what's between the
## <body> </body> tags
# library("rmarkdown")
filename <- "help_page_content/help_page_full"
# render(paste0(filename,".Rmd"), html_document(toc = TRUE, toc_depth = 3))
includeHTML(paste0(filename,".html"))
## Try to strip all but what's in html body 
## some characters in the regex need to be stripped
# system("sed 's/^.*\(<body>.*</body>\).*$/\1/' help_page_full.html > test.txt")