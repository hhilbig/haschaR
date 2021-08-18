#' Felm output summarize - list version
#'
#' @title Felm output summarize - list version
#' @param model_list list of felm models
#' @import tidyverse lfe broom
#' @export


tidy_felm_list <- function(model_list) {
  
  model_list %>% 
    lapply(tidy_felm) %>% 
    reduce(bind_rows)
  
}
