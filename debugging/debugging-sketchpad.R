## debugging-sketchpad.R
## Author: Irena Papst
##
## (Not necessarily coherent) snipets of code written to help with
## debugging rddapp
######################################################################

library(tidyverse)
library(magrittr)

###########
## bug 2 ##
###########

## contingency table for troublesome data
## desired assignment rules
## vp >= 20
## mp <= 5

## load data
data <- read_csv("multivarRD.csv")

## make contingency table
freqs <- data %>%
  mutate(vp_pos = if_else(vp>=20, TRUE, FALSE)) %>%
  mutate(mp_neg = if_else(mp<=5, TRUE, FALSE)) %>%
  group_by(vp_pos, mp_neg) %>%
  summarise(n = n()) %>%
  spread(vp_pos, n)

## look at contingency table counts vs actual treatment
freqs_trt <- data %>%
  mutate(vp_pos = if_else(vp>=20, TRUE, FALSE)) %>%
  mutate(mp_neg = if_else(mp<=5, TRUE, FALSE)) %>%
  group_by(vp_pos, mp_neg, trt) %>%
  summarise(n = n())

###################
## editing plots ##
###################

## load package
library(rddapp)

## generate rd estimates
res <- rd_est(APGAR5 ~ MOMWAIS0, CARE, cutpoint = 85, t.design = "geq")
par(mar = c(6,3,0.5,10))
plot(res)
