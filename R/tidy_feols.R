#' feols output summarize
#'
#' @title feols output summarize
#' @param model feols model
#' @param add_glance if T, glance output will be added to each line (note that not all default glance output will be returned)
#' @param add_dv_stats if T, DV mean, sd, min, max will be added to each line
#' @param add_conf_90 if T, 90\% CIs will be added (note that I use normal and not t distribution)
#' @import tidyverse fixest broom
#' @export


tidy_feols <- function(model, add_glance = T,
                      add_dv_stats = T,
                      add_conf_90 = T) {
  
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  dv <- model.matrix(model, type = 'lhs')
  
  ## Combine and return
  
  out <- m_tidy %>% 
    mutate(n = n)
  
  ## 90% CI
  
  if (add_conf_90) {
    
   out <- out %>% 
     mutate(conf.low90 = estimate - qnorm(0.95) * std.error,
            conf.high90 = estimate + qnorm(0.95) * std.error)
    
  }
  
  ## Add Mean, SD, Min, Max of DV
  
  dv_mean <- dv %>% mean(na.rm = T)
  dv_sd <- dv %>% sd(na.rm = T)
  dv_min <- dv %>% min(na.rm = T)
  dv_max <- dv %>% max(na.rm = T)
  
  ## Add DV stats to output
  
  if (add_dv_stats) {
    
    out <- out %>% 
      mutate(dv_mean = dv_mean,
             dv_sd = dv_sd,
             dv_min = dv_min,
             dv_max = dv_max)
    
  }
  

  
  ## Add model stats
  
  if (add_glance) {
    g <- model %>% broom::glance() %>% 
      dplyr::select(matches('squared|f_'))
    
    out <- out %>% 
      mutate(rsq = g$r.squared,
             a_rsq = g$adj.r.squared)
    
    ## return
    
    out
  } else {
    out
    }
}
