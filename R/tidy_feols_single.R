#' feols output summarize
#'
#' @title feols output summarize
#' @param model feols model
#' @param add_glance if T, glance output will be added to each line (note that not all default glance output will be returned)
#' @param add_dv_stats if T, DV mean, sd, min, max will be added to each line
#' @param add_conf_90 if T, 90\% CIs will be added (note that I use normal and not t distribution)
#' @import tidyverse fixest broom
#' @export


tidy_feols_single <- function(model, add_glance = T,
                              add_dv_stats = T,
                              add_conf_90 = T) {
  
  ## Check if multiple outcome
  
  if (!names(model)[1] == 'nobs') {
    
    cat('Looks like this model uses more than one outcome;\nPlease use tidy_feols instead')
    
  } else {
    
    n <- model$nobs
    m_tidy <- broom::tidy(model, conf.int = T)
    dv_val <- model.matrix(model, type = 'lhs')
    
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
    
    dv_mean <- dv_val %>% mean(na.rm = T)
    dv_sd <- dv_val %>% sd(na.rm = T)
    dv_min <- dv_val %>% min(na.rm = T)
    dv_max <- dv_val %>% max(na.rm = T)
    
    ## Add DV stats to output
    
    if (add_dv_stats) {
      
      out <- out %>% 
        mutate(dv_mean = dv_mean,
               dv_sd = dv_sd,
               dv_min = dv_min,
               dv_max = dv_max)
      
    }
    
    ## Add model formula, variables, FEs
    
    fml <- model$fml
    dv_lab <- fml %>% as.character() %>% 
      {.[2]}
    fml <- Reduce(paste, deparse(fml)) %>% str_squish() %>% 
      str_replace_all('"', '')
    
    out <- out %>% 
      mutate(dv = dv_lab, 
             fml = deparse(fml))
    
    ## 
    
    fe_vars <- paste0(model$fixef_vars, collapse = ", ")
    
    out <- out %>% 
      mutate(fixef_vars = fe_vars)
    
    ## Add model stats
    
    if (add_glance) {
      g <- model %>% broom::glance() %>% 
        dplyr::select(matches('squared|f_'))
      
      out <- out %>% 
        mutate(rsq = g$r.squared,
               a_rsq = g$adj.r.squared)
      
      ## return
      
      out %>% 
        dplyr::select(term, dv, fml, everything())
    } else {
      out %>% 
        dplyr::select(term, dv, fml, everything())
    }
  }
}
