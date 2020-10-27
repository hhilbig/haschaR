#' Remove German umlaut from string 
#'
#' @title Remove German umlaut from string 
#' @param x character string 
#' @import stringr
#' @export

remove_umlaut <- function(x) {
  
  string <- str_replace_all(string, 'ö', 'oe') 
  string <- str_replace_all(string, 'ä', 'ae') 
  string <- str_replace_all(string, 'ü', 'ue') 
  string <- str_replace_all(string, 'ß', 'ss')
  
  return(string)
  
}

