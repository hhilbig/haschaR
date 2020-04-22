#' This is my ggplot theme
#'
#' @param fontsize Selecte fontsize, default is 15
#' @param facet_alt Facet strips with white BG
#' @return This is a custom ggplot theme. You can change the font size and the font family. 
#' @import ggplot2 grDevices
#' @export
#' @examples
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#' geom_point() +
#' theme_hanno(fontsize = 15)
#'
#' 
## theme hanno function

theme_hanno <- function(fontsize = 15,
                        facet_alt = F) {
  
  th <- ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   text = element_text(size = fontsize))
  
  if (facet_alt) {
    th <- th + 
      theme(strip.background =element_blank())+
      theme(strip. = element_text(colour = 'black'))
  }
  
  th
}
