## Summarize a vector 

#' @title Summarize a vector 
#' @param vector numeric vector to summarize
#' @export
#' @examples
#'

summarize_vec <- function(vector){
  
  mean_v <- mean(vector, na.rm = T)
  median_v <- median(vector, na.rm = T)
  sd_v <- sd(vector, na.rm = T)
  n_obs <- length(vector[!is.na(vector)])
  min_v <- min(vector, na.rm = T)
  max_v <- max(vector, na.rm = T)
  
  
  data.frame(cbind(mean_v, median_v, sd_v, n_obs, min_v, max_v))
  
}

