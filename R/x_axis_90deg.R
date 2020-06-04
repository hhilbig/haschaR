#' X axis labels 90 deg 
#'
#' @return X axis labels are now 90 deg 
#' @import ggplot2
#' @export
#' 
## X axis labels 90 deg 

x_axis_90deg <- function() {
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}