#' Returns DF without missings
#'
#' @param df df
#' @param varlist list of vars to be considered for dropping missings
#' @return DF with original string, match and distance to matchs
#' @export

drop_missings_df <- function(df, varlist) {
  df <- df[complete.cases(df[, varlist])]
  df
}