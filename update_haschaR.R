library(devtools)
library(roxygen2)

rm(list = ls())

## Declare WDs

wd_h <- '/Users/hanno/Local_Projects/haschaR/'
wd_s <- '/Users/saschariaz/Google\ Drive_Harvard/Git/haschaR'

## Set WD

haschaR::detect_wd(wd_h = wd_h, wd_alt = wd_s, user_name = 'Hanno')

document()

## PUSH BEFORE REINSTALLING 

devtools::install_github('https://github.com/hhilbig/haschaR', 
                         force = T, upgrade = T)


