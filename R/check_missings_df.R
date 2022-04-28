#' This gives some info on missingness
#'
#' @param df Data frame
#' @param varlist list of variables to check
#' @param group Group for grouped output (leave blank if no group)
#' @import tidyverse
#' @export
#' 

check_missings_df <- function(df, varlist, group = '') {
  
  ## Non grouped
  
  if (group == '') {
    
    out <- df %>% 
      summarise(across(one_of(varlist),
                       ~sum(is.na(.))/n())) %>% 
      mutate(across(one_of(varlist),
                    round, 2))
    row.names(out) <- "Share missing"
    out
  } else {
    
    df[, 'gvar'] <- df[, group]
    
    out <- df %>%
      group_by(gvar) %>% 
      summarise(across(one_of(varlist),
                       ~sum(is.na(.))/n())) %>% 
      mutate(across(one_of(varlist),
                    round, 2))

    out 
  }
}