## Read all worksheets in an Excel workbook into an R list with data.frames

#' @title Read all worksheets 
#' @param filename
#' @param tibble
#' @param ... arguments passed to read_excel
#' @export
#' @examples

read_excel_allsheets <- function(filename, tibble = FALSE, ...) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, ...))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
