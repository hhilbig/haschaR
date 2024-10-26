#' this function checks if the number of observations changes after a left join
#'
#' @param x Left df
#' @param y right df
#' @param ... other arguments to pass to left_join
#' @import tidyverse dplyr
#' @export

left_join_check_obs <- function(x, y, ...) {
  ## Find common variables

  comvars <- colnames(x) %in% colnames(y)
  comvars <- colnames(x)[comvars]

  ## Message

  if (length(comvars) == 0) {
    cat("No common variables found\n")
  } else if (length(comvars) == 1) {
    cat("Common variable: ", comvars, "\n")
  } else {
    cat("Common variables:", paste(comvars, collapse = ", "), "\n")
  }

  ## Check

  nobs1 <- nrow(x)

  out <- dplyr::left_join(x = x, y = y, ...)

  nobs2 <- nrow(out)

  if (nobs1 != nobs2) {
    warning("Number of observations increased from ", nobs1, " to ", nobs2, " after left join")
  } else {
    cat("Number of observations remained the same after left join\n")
  }

  return(out)
}
