#' Left join that fills missings in selected column with same column from right DF
#'
#' @param x left df
#' @param y right df
#' @param y by param (passed to left_join)
#' @param fill_col name of column to be filled from right df
#' @return left df, missings filled in based on right df
#' @import tidyverse
#' @export
#' 


left_join_fill <- function(x, y, by, fill_col = "") {
  
  if (!length(fill_col) == "") {
    
    warning("please only specify one column to be filled")
    
  }
  
  ## Get nrow
  
  nrow_start <- nrow(x)
  
  ## Declare columns to be filled
  
  fcx <- paste0(fill_col, '.x')
  fcy <- paste0(fill_col, '.y')
  
  out <- left_join(x, y, by = by) 
  
  ## Gen temp columns
  
  out[, 'fcx_col'] <- out[, fcx]
  out[, 'fcy_col'] <- out[, fcy]
  
  ## Join and fill
  
  out <- out %>% 
    mutate(!!fill_col := coalesce(fcx_col, fcy_col)) %>% 
      dplyr::select(-fcx_col, -fcy_col, -all_of(c(fcx, fcy)))
           
  ## Get final n
  
  nrow_end <- nrow(out)
  
  if (!nrow_end == nrow_start) {
    
    cat("Careful - merged dataset has different number of rows than original (left) data set.")
    
  }
  
  ## Return
  
  out
  
}
