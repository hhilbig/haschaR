#' this add all units as a separate group, for grouped data
#'
#' @param df Data frame
#' @param group_var the name of the var that defines groups
#' @param new_group_id new id for the group that is composed of all units
#' @import tidyverse
#' @export
#' 

add_all_units_as_group <- function(df, group_var, new_group_id = '') {
  
  df_add <- df
  df_add[, group_var] <- new_group_id
  
  ## Return
  
  df %>% 
    bind_rows(df_add)
  
}
