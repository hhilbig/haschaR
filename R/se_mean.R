## Standard Error of the Mean

#' @title Standard Error of the Mean
#' @param var Variable
#' @export
#' @examples
#'

se_mean <- function(var){
  var_eff <- var[!is.na(var)]
  sd(var_eff) / sqrt(length(var_eff))
}
