#' function to convert ags from old to new 
#'
#' @param ags_old_input OLD AGS back to 2006
#' @return 2022 AGS 
#' @import tidyverse
#' @export
#' @examples
#'

reformen <- read.csv("https://www.dropbox.com/s/a0mj3yb9fvgyohc/ags_old_new_conversion.csv?dl=1",
                     colClasses = 'character') 

ags_old_to_new <- function(ags_old_input){
  
  if (! "tidyverse" %in% loadedNamespaces()) {
    stop("tidyverse needed for this function to work. Please load/install it.",
         call. = FALSE)
  }
  
  res <- reformen %>%
    filter(ags_old == ags_old_input)
  
  ## If no match is found, return old ags (no change)
  
  if(nrow(res) == 0){
    
    return(ags_old_input)
    
  }
  
  ## Else return new ags 
  
  else(
    
    return(res %>% pull(ags_new) %>% as.character(.))
    
  )
  
}
