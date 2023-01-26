#' feols output summarize for multiple outcomes (should also work for just one outcome)
#'
#' @title feols output summarize for multiple outcomes
#' @param model feols model with multiple outcomes
#' @param ... arguments passed to tidy_feols_single (see documentation)
#' @import tidyverse fixest broom
#' @export


tidy_feols <- function(model, ...) {
  
  if (!is.null(names(model))) {
    
    tidy_feols_single(model, ...)
    
  } else {
    
    lapply(model, function(x) {
      tidy_feols_single(x, ...)
    }) %>% reduce(bind_rows)
  }
}
