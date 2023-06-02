## Convert one PDF file to a PNG file

#' @title PDF to PNG
#' @param pdf_file_path PDF file path
#' @param output_dir Directory where PNG should go
#' @param dpi DPI
#' @import pdftools magick png
#' @export

pdf_to_png <- function(pdf_file_path, output_dir, dpi = 600) {
  
  ## Fname
  
  png_filename <- paste0(output_dir, "/", 
                         tools::file_path_sans_ext(basename(pdf_file_path)), 
                         ".png")
  
  # Do
  bitmap <- pdf_render_page(pdf_file_path, page = 1, dpi = dpi)
  writePNG(bitmap, png_filename)
  
}
