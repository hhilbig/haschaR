#' Felm output summarize
#' Main diff. to normal tidy is that this also gets number of obs
#'
#' @title Felm output summarize
#' @param model felm model
#' @param add_glance if T, glance output will be add to each line (note that not all default glance output will be returned)
#' @import tidyverse lfe broom
#' @export


tidy_felm <- function(model, add_glance = T) {
  
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  
  ## Combine and return
  
  out <- m_tidy %>% 
    mutate(n = n)
  
  if (add_glance) {
    g <- model %>% glance() %>% 
      dplyr::rename(f_stat = statistic,
                    f_pval = p.value) %>% 
      dplyr::select(matches('squared|f_'))
    
    out <- out %>% 
      mutate(rsq = g$r.squared,
             a_rsq = g$adj.r.squared,
             f_stat = g$f_stat,
             f_pval = g$f_pval)
    
    ## return
    
    out
  } else {
    out
    }
}
