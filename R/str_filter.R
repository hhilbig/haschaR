#' Takes string, looks for expr and then finds matches and returns them
#'
#' @param string list of felm models to be passed to stargazer 
#' @param pattern name of explanatory variable to be replaced
#' @param neg if T, negate
#' @return Returns string
#' @import tidyverse stringr
#' @export

str_filter <- function(string, pattern, neg = F) {
  
  if (neg == F) {
    
    string %>% .[str_detect(., pattern)]
    
  } else {
    
    string %>% .[!str_detect(., pattern)]
    
  }
  
}