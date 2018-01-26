library(haven)
library(dplyr)
library(magrittr)
library(devtools)

#care <- read_sav("C:\\Users\\fjt36-admin\\Dropbox\\WORK\\2016 Winter Cornell\\rdd software jebs\\ICPSR_04091\\data.sav")
care <- read_sav("C:/Users/Ze/Dropbox (Cornell University)/Cornell/RDD/ICPSR_04091/data.sav")

care <- care %>%
        select(subject,dc_trt,momwais0,hri0,apgar5,sbiq48)  %>% 
        mutate(rdd_trt = ifelse(momwais0 > 85,1,0)) %>%
        mutate(sbiq48_rdd = ifelse(rdd_trt==dc_trt,sbiq48,NA))

setwd('C:/Academia/Cornell/QML/RDD/RDD/rdd')

devtools::use_data(care, pkg = 'rddapp', overwrite = TRUE)
