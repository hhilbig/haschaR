#' pad strings of length X with leading zeroes or any other char
#'
#' @param str string
#' @param str_len nchar of string for which 0s or other char will be added
#' @param char_pad char to be added, default 0
#' @return returns a string vector
#' @import tidyverse stringr
#' @export
#' 

pad_zero_conditional <- function(str,
                                 str_len = 7,
                                 char_pad = '0') {
  
  ## Convert input to string, add 0, return
  str %>% as.character() %>% 
    ifelse(nchar(.) == str_len, paste0(char_pad, .), .)
  
}
