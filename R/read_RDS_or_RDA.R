#' Read a single object from an RDS or RDA file
#'
#' This is like [readRDS()] except that it can also read from an RData
#' file containing a single object (i.e. the kind of file that is read
#' using [load()]). Use this in place of `readRDS()` if you want to be
#' slightly more forgiving about what kind of R data file you accept.
#'
#' @param filename The file to read an R object from.
#' @param expected.class If specified, the object will be coerced into
#'     this class using `as()`, which will throw an error as normal if
#'     the coercion is not possible. This allows you to restrict what
#'     kind of objects you will accept. This can also be a function
#'     that accepts a single argument and performs the proper coercion
#'     itself.
#' @return The object read from the file, possibly after coercing it
#'     into another class.
#'
#' @examples
#'
#' tmpf <- tempfile()
#' saveRDS(1:10, tmpf)
#' read_RDS_or_RDA(tmpf)
#' read_RDS_or_RDA(tmpf, "character")
#' # Using a function instead of a class name.
#' read_RDS_or_RDA(tmpf, as.character)
#' read_RDS_or_RDA(tmpf, "factor")
#' read_RDS_or_RDA(tmpf, "data.frame")
#'
#' \dontrun{
#' # This will throw an error because the coercion to "lm" is not
#' # possible.
#' read_RDS_or_RDA(tmpf, "lm")
#' }
#'
#' @seealso [readRDS()], [read_single_object_from_rda()], [as()]
#'
#' @importFrom glue glue
#' @export
read_RDS_or_RDA <- function(filename, expected.class = "ANY") {
  object <- suppressWarnings(tryCatch({
    readRDS(filename)
  }, error = function(...) {
    read_single_object_from_rda(filename)
  }))
  if (is.function(expected.class)) {
    object <- do.call(expected.class, list(object))
  } else if (!is(object, expected.class)) {
    ## Try to use as.[CLASS] if it exists. If not use as(.,
    ## "[CLASS]").
    coerce_func <- tryCatch(
      get(glue("as.{expected.class}")),
      error = function(...) . %>% as(expected.class))
    object <- coerce_func(object)
  }
  return(object)
}
