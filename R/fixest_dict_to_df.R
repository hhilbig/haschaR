#' Convert fixest dict to DF (requires that fixest dict has been set before)
#'
#' @import fixest
#' @export
#' 

fixest_dict_to_df <- function() {
  
  ## check if fixest dict exists
  
  if (length(getFixest_dict()) < 2) {
    cat("Dict only has one entry - check if dict is loaded properly")
    
    df <- getFixest_dict() %>% 
      data.frame(varname = names(.), label = .)
    rownames(df) <- NULL
    df
    
  } else {
    
    df <- getFixest_dict() %>% 
      data.frame(varname = names(.), label = .)
    rownames(df) <- NULL
    df
    
  }
  
}
