#' Felm output summarize
#'
#' @title Felm output summarize
#' @param model felm model
#' @param add_glance if T, glance output will be added to each line (note that not all default glance output will be returned)
#' @param add_dv_stats if T, DV mean, sd, min, max will be added to each line
#' @param add_conf_90 if T, 90% CIs will be added (note that I use normal and not t distribution)
#' @import tidyverse lfe broom
#' @export


tidy_felm <- function(model, add_glance = T,
                      add_dv_stats = T,
                      add_conf_90 = T) {
  
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  
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
  
  dv <- model$formula %>% str_split(' ~ ', simplify = T) %>% .[2]
  dv_mean <- model %>% augment() %>% pull(!!dv) %>% mean(na.rm = T)
  dv_sd <- model %>% augment() %>% pull(!!dv) %>% sd(na.rm = T)
  dv_min <- model %>% augment() %>% pull(!!dv) %>% min(na.rm = T)
  dv_max <- model %>% augment() %>% pull(!!dv) %>% max(na.rm = T)
  
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
