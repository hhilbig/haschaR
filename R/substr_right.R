## Extracting the last n characters from a string in R

#' @title Extracting the last n characters from a string in R
#' @param x
#' @param n
#' @export
#' @examples
#' substrRight(x, 6)
#' "string"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
