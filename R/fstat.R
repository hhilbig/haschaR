## Extracting the F-stat from an IVreg Model

#' @title Extracting the F-stat from an IVreg Model
#' @param model
#' @export
#' @examples
#'

fstat_extract <- function(model){
  return(c(summary(model, diagnostics = T)$diagnostic[1,3]))
}
