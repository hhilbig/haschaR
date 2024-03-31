#' Convert german party names (de or en) to proper German party names
#'
#' @param party_vec vector of party names
#' @param far_right_label label for far-right parties
#' @param linebreak_far_right add linebreak for far right label
#' @import tidyverse stringr
#' @export
#'

german_party_convert <- function(
    party_vec,
    far_right_label = "Far-right parties (incl. AfD)", 
    linebreak_far_right = T) {

  if (linebreak_far_right) {

    far_right_label <- "Far-right parties\n(incl. AfD)"

  }
      
  ## Party origins and destinations

  party_orig <- list(
    c("spd", "SPD"),
    c("CDU", "cdu", "csu", "c.s.u", "christ"),
    c(
      "LINKE", "linkspartei", "pds", "links",
      "left"
    ),
    c("greens", "Gruene", "green", "gruen"),
    c("fdp", "f.d.p."),
    c("AFD", "afd", "a.f.d.", "alternative"),
    c("far right", "far-right", "far_right")
  )
  party_destination <- c("SPD", "CDU/CSU", "Left", "Greens", "FDP", "AfD", far_right_label)

  ## Loop

  party_vec %>%
    sapply(function(x) {
      which_party <- sapply(party_orig, function(p) {
        str_detect(tolower(x), paste0(tolower(p), collapse = "|"))
      })

      ## If nothing found, return the original value

      if (all(!which_party)) {
        x
      } else {
        party_destination[which_party]
      }
    }) %>%
    unname()
}
