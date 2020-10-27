#' Remove German umlaut from string 
#'
#' @title Remove German umlaut from string 
#' @param x character string 
#' @import stringr
#' @export

remove_umlaut <- function(x) {
  
  x <- str_replace_all(x, 'ö', 'oe') 
  x <- str_replace_all(x, 'ä', 'ae') 
  x <- str_replace_all(x, 'ü', 'ue') 
  x <- str_replace_all(x, 'ß', 'ss')
  
  
  x <- str_replace_all(x, 'Ö', 'Oe') 
  x <- str_replace_all(x, 'Ä', 'Ae') 
  x <- str_replace_all(x, 'Ü', 'Ue') 
  
  return(x)
  
}

