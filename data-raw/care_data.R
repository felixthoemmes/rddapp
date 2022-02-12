# The dataset is available online at the following URL:
# https://www.icpsr.umich.edu/web/ICPSR/studies/4091
# The file is ASCII, but has NO column labels and value labels
# Therefore we also downloaded the .sps file from the same website, 
# and executed the SPSS syntax file
# 04091-0001-Setup.sps
# to create a .sav file called
# 04091-0001-Data.sav

# This file was then read into R and modified
library(haven)
care_raw <- read_sav("C:\\Users\\fjt36\\Dropbox\\WORK\\2017 Winter Cornell\\CARE dataset\\04091-0001-Data.sav")

# In a next step we subset the data
# then generate a treatment assignment that would be observed under a hypothetical RDD
# and then we generate the outcome variable
library(dplyr)
library(tidyr)
library(magrittr)
care_combined <- care_raw %>% 
  select(SUBJECT, DC_TRT, MOMWAIS0, HRI0, APGAR5, SBIQ48)  %>% 
  mutate(RDD_TRT = ifelse(MOMWAIS0 < 85, 1, 0)) %>%
  mutate(SBIQ48_RDD = ifelse(RDD_TRT == DC_TRT, SBIQ48, NA))

# In a last step we subset the data further to remove any NAs on the outcome
# and then remove temporary variables created in the previous step
CARE <- subset(care_combined, !is.na(SBIQ48_RDD))
CARE <- CARE %>% select(SUBJECT, DC_TRT, MOMWAIS0, HRI0, APGAR5, SBIQ48)
