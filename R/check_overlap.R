#' Checks overlap between two vectors (left and right sided)
#'
#' @param vec_x Vector
#' @param vec_y Vector
#' @param return_miss If T, return list of vec_x missing from vec_y
#' @export
##

check_overlap <- function(vec_x, vec_y, return_miss = F) {
  
  ## Make unique 
  
  vec_x <- unique(vec_x); vec_y <- unique(vec_y)
  
  q1 <- sum(vec_x %in% vec_y) / length(vec_x)
  cat("Share values of first vector in second vector:", round(q1, 3),
      '\n')
  
  q2 <- sum(vec_y %in% vec_x) / length(vec_y)
  cat("Share values of second vector in first vector:", round(q2, 3))
  
  if (return_miss) {
    
    cat("\nAlso returning values from first vec that are missing in second vec")
    m <- vec_x[!vec_x %in% vec_y]
    m
  }
}
  