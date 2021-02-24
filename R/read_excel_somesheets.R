## Read some worksheets in an Excel workbook into an R list with data.frames

#' @title Read all worksheets 
#' @param filename
#' @param tibble
#' @param string_find this is the string that is used for filtering sheet names
#' @param ... arguments passed to read_excel
#' @export
#' @examples

read_excel_somesheets <- function(filename, tibble = FALSE, string_find = '', ...) {
  sheets <- readxl::excel_sheets(filename)
  
  sheets_use <- sheets[str_detect(sheets, string_find)]
  
  x <- lapply(sheets_use, function(X) readxl::read_excel(filename, sheet = X, ...))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets_use
  x
}
