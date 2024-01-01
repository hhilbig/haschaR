#' find and run MPTO script in a project without having to run script manually
#'
#' @param code_directory_string this should be a strong that identifies the code directory
#' @param script_string this should be a string that identifies the MPTO script in the code directory
#' @return returns nothing but runs the script
#' @import tidyverse stringr
#' @export
#' 

move_plots_to_overleaf <- function(code_directory_string = 'Code',
                                   script_string = 'Overleaf') {
  
  d <- dir() %>% 
    str_filter(code_directory_string) %>%
    str_filter("workspace", neg = T) %>% 
    first()
    dir(recursive = T, full.names = T) %>% 
    str_filter(script_string) %>% 
    first()
  
  if (!is.na(d) | d == '' | is.null(d)) {
    
    cat('Move plots to overleaf script found as', d, 
        '\nRunning script...\n')
    
    source(d)
    
  } else {
    
    cat('Move plots to overleaf script not found')
    
  }

}
