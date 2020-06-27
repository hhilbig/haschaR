#' add linebreaks to strings more than n characters in length, vectorized
#'
#' @param string string vector
#' @param ... arguments passed to add_linebreak
#' @return returns a string vector 
#' @import tidyverse
#' @export
#' 

add_linebreak_vector <- function(string, ...) {
  sapply(string, function(s) add_linebreak(s, ...))
}
  