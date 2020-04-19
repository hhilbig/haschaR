## Assign non-unique numeric ranks 

#' @title Assign non-unique numeric ranks 
#' @param x numeric vector 
#' @export
#' @examples
#' 
#' 

rank_fun <- function(x){
  su = sort(unique(x))
  for (i in 1:length(su)) x[x==su[i]] = i
  return(x)
}
