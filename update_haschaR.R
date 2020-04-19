library(devtools)
library(roxygen2)
setwd()


## Declare WDs

wd_h <- ''
wd_s <- '/Users/saschariaz/Google\ Drive_Harvard/Git/haschaR'

## Set WD

haschaR::detect_wd(wd_h = wd_h, wd_alt = wd_s, user_name = 'Hanno')

document()

## PUSH BEFORE REINSTALLING 

install_github('https://github.com/saschariaz/saschaR', force = T)

