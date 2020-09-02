## Summarize the output from felm
## Main diff. to normal tidy is that this also gets number of obs

#' @title Summarize the output from felm
#' @param model felm model
#' @import tidyverse lfe
#' @export
#' @examples
#'
#'

tidy_rd <- function(model){
  
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  
  ## Combine and return
  
  m_tidy %>% 
    mutate(n = n)
}
