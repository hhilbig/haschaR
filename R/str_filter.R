#' Takes string, looks for expr and then finds matches and returns them
#'
#' @param string list of felm models to be passed to stargazer 
#' @param pattern name of explanatory variable to be replaced
#' @return Returns string
#' @import tidyverse stringr
#' @export

str_filter <- function(string, pattern) {
  
  string %>% .[str_detect(., pattern)]
  
}