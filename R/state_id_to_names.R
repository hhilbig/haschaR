## Conversion between German state IDs (AGS) and English names 
#' @title Conversion between German state IDs (AGS) and English names 
#' @param state_id state id vector 
#' @export
#' @examples
#'
#'

state_id_to_names <- function(state_id){
  
  names <- recode(state_id,
                  `01` = 'Schleswig-Holstein',
                  `02` = 'Hamburg',
                  `03` = 'Niedersachsen',
                  `04` = 'Bremen',
                  `05` = 'North Rhine-Westphalia',
                  `06` = 'Hesse',
                  `07` = 'Rhineland-Palatinate',
                  `08` = 'Baden-Württemberg',
                  `09` = 'Bavaria',
                  `10` = 'Saarland',
                  `11` = 'Berlin',
                  `12` = 'Brandenburg',
                  `13` = 'Mecklenburg-Vorpommern',
                  `14` = 'Saxony',
                  `15` = 'Saxony-Anhalt',
                  `16` = 'Thuringia')
  
  return(names)
  
}
