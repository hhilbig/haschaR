#' This gives some info on missingness, but as a plot
#'
#' @param df Data frame
#' @param varlist list of variables to check
#' @param group Group for grouped output (leave blank if no group)
#' @import tidyverse
#' @export
#' 

check_missings_plot <- function(df, varlist, group = '') {
  
  ## Non grouped
  
  if (group == '') {
    
    out <- df %>% 
      summarise(across(one_of(varlist),
                       ~sum(is.na(.))/n())) %>% 
      mutate(across(one_of(varlist),
                    round, 8))
    ## Plot 
    
    ## plot
    
    out %>% 
      pivot_longer(cols = everything()) %>% 
      ggplot(aes_string(x = "name", y = "value")) + 
      geom_bar(stat = "identity") + 
      ylab("Share missing") +
      theme_hanno() +
      x_axis_90deg()
    
  } else {
    
    df[, 'gvar'] <- df[, group]
    
    out <- df %>%
      group_by(gvar) %>% 
      summarise(across(one_of(varlist),
                       ~sum(is.na(.))/n())) %>% 
      mutate(across(one_of(varlist),
                    round, 8))
    
    ## Plot this
    
    out[, group] <- out[, "gvar"]
    
    ## plot
    
    out %>% 
      dplyr::select(-gvar) %>% 
      pivot_longer(cols = -c(group)) %>% 
      ggplot(aes_string(x = group, y = "value")) + 
      geom_bar(stat = "identity") + 
      facet_wrap(~name) + 
      ylab("Share missing") +
      theme_hanno() +
      x_axis_90deg()
  }
}
