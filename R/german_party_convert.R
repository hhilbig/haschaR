#' Convert german party names (de or en) to proper German party names
#'
#' @param party_vec vector of party names
#' @import tidyverse stringr
#' @export
#' 

german_party_convert <- function(party_vec) {
  
  ## Party origins and destinations
  
  party_orig <- list(c('spd', 'SPD'),
                     c('CDU', 'cdu', 'csu', 'c.s.u', 'christ'),
                     c('LINKE', 'linkspartei', 'pds', 'links',
                       'left'),
                     c('greens', 'Gruene', 'green', 'gruen'),
                     c('fdp', 'f.d.p.'),
                     c('AFD', 'afd', 'a.f.d.', 'alternative'))
  party_destination <- c('SPD', 'CDU/CSU', 'Left', 'Greens', 'FDP', 'AfD')
  
  ## Loop
  
  party_vec %>% sapply(function(x) {
    
    which_party <- sapply(party_orig, function(p) {
      
      str_detect(tolower(x), paste0(tolower(p), collapse = '|'))
      
    })
    
    ## If nothing found, return the original value
    
    if (all(!which_party)) {
      x
    } else {
      party_destination[which_party]
    }
    
  }) %>% unname()                   
}