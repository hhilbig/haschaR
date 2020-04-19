#' Harmonized variable names for stargazer output for felm models
#'
#' @param model_list list of felm models to be passed to stargazer 
#' @param term_replace name of explanatory variable to be replaced
#' @param term_keep replacement name for the variable to be replaced
#' @return This returns the list of models, with harmonized variable names
#' @import lfe
#' @export

harmonize_dv_felm <- function(model_list, 
                              term_replace = '', 
                              term_keep = '') {
  
  ## ## Only works for felm right now 
  
  for (j in seq_along(model_list)) {
    
    m <- model_list[[j]]
    
    if (term_replace %in% row.names(m$coefficients)) {
      
      ## Replace the term
      
      row.names(m$coefficients)[row.names(m$coefficients) == term_replace] <- 
        term_keep
      row.names(m$beta)[row.names(m$beta) == term_replace] <- 
        term_keep
      
      ## Add model back to list
      
      model_list[[j]] <- m
      
    }
  }
  

  ## Return list
  
  model_list
  
}