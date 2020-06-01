#' add linebreaks to strings more than n characters in length
#'
#' @param string string
#' @param min_length minimum length for string to be broken into two lines
#' @param add_multiple_linebreaks if set to T, linebreaks will be added to all but the first whitespace or dash
#' @return returns a string
#' @import tidyverse
#' @export
#' 

add_linebreak <- function(string,
                          min_length = 10,
                          add_multiple_linebreaks = F) {
  
  if (nchar(string) > min_length) {
    
    if (!add_multiple_linebreaks) {
      
      ##
      l <- nchar(string)
      find_space <- str_locate_all(string, ' |\\-') %>%
        .[[1]] %>% data.frame() %>%
        pull(start) %>% .[which.min(abs(. - (nchar(string) / 2)))]
      
      ## add line break
      substr(string, find_space, find_space) <- '\n'
      string
    } else {
      find_space <- str_locate_all(string, ' |\\-') %>%
        .[[1]] %>% data.frame() %>% slice(-1) %>% 
        pull(1)
      
      for (i in find_space) {
        substr(string, i, i) <- '\n'
      }
      string
    }
  } else {
    string
  }
}
  