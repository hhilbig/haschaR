library(devtools)
library(roxygen2)

rm(list = ls())

## Declare WDs

wd_h <- '~/Documents/GitHub/haschaR/'
wd_s <- '/Users/saschariaz_1/Dropbox\ (Nuffield\ College)/07_Git/haschaR'

## Set WD

haschaR::detect_wd(wd_h = wd_h, wd_alt = wd_s, user_name = 'hanno',
                   wd_h2 = wd_h2)

## For some reasons some dependencies are not installed w/ the package
## This should fix this:

use_package("lfe")
use_package("stringdist")
use_package("stringr")
use_package("ggplot2")
use_package("dplyr")
use_package("fixest")
use_package("broom")
use_package("rdrobust")
use_package("clipr")
use_package("grDevices")
use_package("glue")
use_package("magick")
use_package("pdftools")
use_package("png")

document()

## PUSH BEFORE REINSTALLING 

devtools::install_github('hhilbig/haschaR', 
                         upgrade = T, force = T, quiet = F)

