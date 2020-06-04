#' X axis labels 45 deg 
#'
#' @return X axis labels are now 45 deg 
#' @import ggplot2
#' @export
#' 
## X axis labels 45 deg 

x_axis_45deg <- function() {
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}