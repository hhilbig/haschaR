#' Felm output summarize
#' Main diff. to normal tidy is that this also gets number of obs
#'
#' @title Felm output summarize
#' @param model felm model
#' @import tidyverse lfe broom
#' @export


tidy_felm <- function(model) {
  
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  
  ## Combine and return
  
  m_tidy %>% 
    mutate(n = n)
}

