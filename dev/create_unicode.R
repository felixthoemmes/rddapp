library(stringi)

# find the special unicode escape format
x <- "This is a bullet \u2022"
y <- "This is a bullet •"
identical(x, y)
## [1] TRUE
cat(stringi::stri_escape_unicode(x))
## This is a bullet \u2022
