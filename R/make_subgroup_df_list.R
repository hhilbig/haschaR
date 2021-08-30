#' Takes sdata frame and list of vars (categorical), and them makes a list of data frame
#'
#' @param htevars list of vars that data sets will be based on
#' @param data data frame
#' @return Returns  list of data sets and labels
#' @import tidyverse
#' @export

make_hte_dfs <- function(htevars, data) {
  
  ## First, check if hte vars are categorical
  
  htevar_vals <- sapply(htevars, function(x) length(unique(data[, x])))
  
  ## If those have less than 6 values, fine - if not, stop
  
  if (any(htevar_vals > 5)) {
    
    stop('Looks like variables arent categorical - fix this')
    
  }
  
  out <- lapply(htevars, function(x) {
    
    ## First, get all the values 
    
    vals_htevar <- unique(data[,x]) %>% 
      na.omit()
    
    ## Then make a list of datasets conditional on this value
    
    l <- lapply(vals_htevar, function(v) {
      
      data[data[, x] == v, ]
    })
    
    l
    
  }) %>% reduce(c)
  
  labs <- lapply(htevars, function(x) {
    
    ## First, get all the values 
    
    vals_htevar <- unique(data[,x])
    paste0(x, '_', vals_htevar)
  }) %>% reduce(c)
  
  ## Return this
  
  list('data' = out, 'labels' = labs)
}
