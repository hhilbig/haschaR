#' Convert cont var to binary
#'
#' @title Convert cont var to binary
#' @param v method q
#' @param v variable (must be continuous)
#' @param method mean, median or quantile split
#' @param q if method = quantile, then this is the quantile that is used (0-1)
#' @import tidyverse
#' @export


make_binary <- function(v, 
                        method = 'median',
                        q = 0.75) {
  
  if (!(method %in% c('mean', 'median', 'quantile'))) {
    break("Unknown method")
  }
  
  if (!method == 'quantile') {
    
    cutoff = ifelse(method == 'mean', 
                    mean(v, na.rm = T),
                    median(v, na.rm = T))
    
    b <- ifelse(v > cutoff, 1, 0)
    
  } else {
    
    cutoff = quantile(v, probs = q, na.rm = T)
    
    b <- ifelse(v > cutoff, 1, 0)
    
  }
  
  ## return
  
  b
}