#' Fuzzy string merging function
#'
#' @param string_vec1 vector of strings
#' @param string_vec1 vector of strings
#' @param method distance calculation method, based on methods in stringdist
#' @return DF with original string, match and distance to matchs
#' @import stringdist
#' @import tidyverse
#' @export


fuzzy_string_match <- function(string_vec1, string_vec2, method = 'jw') {
  
  out <- lapply(string_vec1, function(s) {
    
  ## Distance
  
  d <- stringdist(s, string_vec2, method = method)
  
  ## Get the match
  
  m <- string_vec2 %>% .[which.min(d)]

  ## Return this with distance 
  
  data.frame(orig_string = s,
             match_string = m, 
             dist_string = min(d, na.rm = T), 
             stringsAsFactors = F)
  }) %>%
    reduce(rbind)
  
  ## Return
  
  out
}
