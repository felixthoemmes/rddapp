## Attempt 1: no TOC
##includeMarkdown("help_page_content/help_page_full.Rmd")

## Attempt 2: works if we only include what's between the
## <body> </body> tags
# library("rmarkdown")

# filename <- "help_page_content/help_page_full"

# render(paste0(filename,".Rmd"), html_document(toc = TRUE, toc_depth = 3))

# includeHTML(paste0(filename,".html"))

## Try to strip all but what's in html body 
## some characters in the regex need to be stripped
# system("sed 's/^.*\(<body>.*</body>\).*$/\1/' help_page_full.html > test.txt")

## Attempt 3: Load HTML header and body separately using tags function
# includeHTML('help_page_content/help_page_full.html')
# tags$head(includeHTML("help_page_content/help_head.html"))
# tags$body(includeHTML("help_page_content/help_body.html"))
# tags$body(includeHTML("help_page_content/help_page_full.html"))

## Attempt 4: read HTML as character string from .txt file
filename <- "help_page_content/help_head.txt"
tags$head(HTML(readChar(filename, file.info(filename)$size)))
filename <- "help_page_content/help_body.txt"
tags$body(HTML(readChar(filename, file.info(filename)$size)))