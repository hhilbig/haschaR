#' Fuzzy string join function
#'
#' @param df_left left df
#' @param df_right right df
#' @param by name of string to be matched onstringdist
#' @param quantile_dist_not_match q of distribution of distance such that greater distances will not be matched
#' @return left df with matched stuff from right df 
#' @import stringdist
#' @import tidyverse
#' @export

fuzzy_string_join <- function(df_left, df_right, by = '',
                              quantile_dist_not_match = 0.5) {
  
  ## Check if by is in both DFs
  
  if (!((by %in% colnames(df_left)) & (by %in% colnames(df_right)))) {
    stop('By variable needs to be in both data frames')
  }
  
  ## Get what we need
  
  sv1 <- df_left %>% pull(!!by) %>% as.character() %>% 
    unique()
  sv2 <- df_right %>% pull(!!by) %>% as.character() %>% 
    unique()
  
  ## Get quantile above which we will not match
  
  fsm <- fuzzy_string_match(sv1, sv2) 
  
  q <- quantile(fsm$dist_string, 
                quantile_dist_not_match, 
                na.rm = T)
  
  cat('Strings with distance greater than', q, 'will not be matched')
  
  ## Remove these matches
  
  fsm <- fsm %>% 
    mutate(match_string = ifelse(dist_string > q, NA_character_, 
                                 match_string)) %>% 
    dplyr::rename(!!by := orig_string) %>% 
    dplyr::select(-dist_string)
           
  ## Do the final matching
  
  df_final <- df_left %>% 
    left_join(fsm) %>% 
    left_join(df_right %>% dplyr::rename(match_string := !!by)) %>% 
    dplyr::select(-match_string)
  
  ## Return
  
  df_final
  
}