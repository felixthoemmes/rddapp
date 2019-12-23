# load packages
library(rddapp)

# devtools::load_all("rddapp")
# devtools::install_github("wliao229/RDD",
# 	auth_token = '97d442b7543326db9a2c182c3ea83b8e3e24d5c1')

# global parameters
MORE_REACTIVE <- T
options(shiny.maxRequestSize = 9*1024^2) 
'%then%' <- shiny:::'%OR%'

# load modules
for (f in list.files(file.path('modules'), pattern = '\\.R$', recursive = T, full.names = T))
  source(f)

# load utility functions
for (f in list.files(file.path('utilities'), pattern = '\\.R$', recursive = T, full.names = T))
  source(f)

# format p value
fmt_p <- Vectorize(
  function(x) {
    if (is.na(x)) return('')
    if (x < .001) return("<.001") 
    else return(sub('0.', '.', sprintf('%.3f', x), fixed = T))
  }
)

get_columns <- function(df, cols){
  req(all(cols %in% names(df)))
  df[, cols]
}
