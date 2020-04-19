#' This is my ggplot theme
#'
#' @param fontsize Selecte fontsize, default is 15
#' @param font Selected font. Either 'default' or 'frutiger'.
#' @return This is a custom ggplot theme. You can change the font size and the font family. 
#' @import ggplot2 grDevices
#' @export
#' @examples
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#' geom_point() +
#' theme_hanno(fontsize = 15, font = 'frutiger')
#'
#' 
## theme hanno function

theme_hanno <- function(fontsize = 15, font = 'default') {
  
  ## Check if selected font exists 
  
  if (!(font %in% c('default', 'frutiger'))) {
    
    stop('Change font argument to <default> or <frutiger>')
  
  } else {
    
    if (font == 'frutiger') {
      
      cat('Frutiger font needs to be installed for this to work\n')
      cat('Caution: This might not work on a Mac')
      
      windowsFonts("Frutiger-Light" = windowsFont("Frutiger-Light"))
      
      ## Do theme 
      
      ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       text = element_text(size = fontsize,
                                                           family = "Frutiger-Light"))
      
    } else {
      
      ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = fontsize))
    }
  }
}
