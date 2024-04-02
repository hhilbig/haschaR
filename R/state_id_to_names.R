## Conversion between German state IDs (AGS) and English names 
#' @title Conversion between German state IDs (AGS) and English names 
#' @param state_id state id vector 
#' @param how character, either "full_name" or "short_name"
#' @export
#' @examples
#'
#'

state_id_to_names <- function(state_id, how = "full_name"){

  ## Check if how is either "full_name" or "short_name"

  if (!how %in% c("full_name", "short_name")) {
    stop("how must be either 'full_name' or 'short_name'")
  }

  ## Recode state_id to names

  if (how == "full_name") {
    names <- recode(state_id,
      `01` = "Schleswig-Holstein",
      `02` = "Hamburg",
      `03` = "Niedersachsen",
      `04` = "Bremen",
      `05` = "North Rhine-Westphalia",
      `06` = "Hesse",
      `07` = "Rhineland-Palatinate",
      `08` = "Baden-Württemberg",
      `09` = "Bavaria",
      `10` = "Saarland",
      `11` = "Berlin",
      `12` = "Brandenburg",
      `13` = "Mecklenburg-Vorpommern",
      `14` = "Saxony",
      `15` = "Saxony-Anhalt",
      `16` = "Thuringia"
    )
  }

  ## Recode state_id to short names
  
  if (how == "short_name") {
    names <- recode(state_id,
      `01` = "SH",
      `02` = "HH",
      `03` = "NI",
      `04` = "HB",
      `05` = "NW",
      `06` = "HE",
      `07` = "RP",
      `08` = "BW",
      `09` = "BY",
      `10` = "SL",
      `11` = "BE",
      `12` = "BB",
      `13` = "MV",
      `14` = "SN",
      `15` = "ST",
      `16` = "TH"
    )
  }

  return(names)
  
}
