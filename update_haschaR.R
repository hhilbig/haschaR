library(devtools)
library(roxygen2)

rm(list = ls())

## Declare WDs

wd_h <- '~/Documents/GitHub/haschaR/'
wd_s <- '/Users/saschariaz_1/Dropbox\ (Nuffield\ College)/07_Git/haschaR'

## Set WD

haschaR::detect_wd(wd_h = wd_h, wd_alt = wd_s, user_name = 'hanno',
                   wd_h2 = wd_h2)

document()

## PUSH BEFORE REINSTALLING 

devtools::install_github('hhilbig/haschaR', 
                         upgrade = T, force = T, quiet = T)

## 


