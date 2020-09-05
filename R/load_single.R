#' A function to load a single object from an R data file
#'
#' \code{load()} loads all objects in the data file which is fine for most of the uses.
#' However, one may wish to load only a single object, if, for example, one has conflicting object names in the same environment.
#' This function also allows the user to rename an object within the data frame.
#' Unfortunately, it is not possible to selectively load objects so this function loads every thing in the data file, and then drops what is not required.
#' Cribbed from https://stackoverflow.com/questions/8700619/get-specific-object-from-rdata-file
#'
#' @param file An .RData file
#' @param object An object known to be saved within the RData file
#' @param rename String, a new name for the object to take
#'
#' @examples
#' x <- 1
#' y <- 2
#' save(x,y, file = paste0(tempdir(), "/temp.RData"))
#' rm(x, y)
#' nice_load(file = paste0(tempdir(), "/temp.RData"), "y")
#' nice_load(file = paste0(tempdir(), "/temp.RData"), "y", rename = "z")
#' @export

nice_load <- function(file, object, rename = NULL){
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # assertthat::assert_that(is.character(file), "file must be a string")
  # assertthat::assert_that(is.character(object), "object must be a string")
  # assertthat::assert_that((is.character(rename) | is.null(rename)), "rename must be a string or NULL")
  
  file_string <- stringr::str_replace(file, "^.*/", "")
  file_string <- stringr::str_replace(file, "\\.RData", "")
  
  # get data frame into local environment
  e = local({load(file); environment()})
  
  # make lazy-load database
  tools:::makeLazyLoadDB(e, file_string)
  lazyLoad(file_string)
  
  # load object
  get(object)
  
  if(!is.null(rename) ){
    # create object in local env that has name matching value for object, with new name same as rename
    assign(eval(rename), get(object), envir = .GlobalEnv)
    # assign(ls()[ls() == eval(object)], rename)
    rm(e)
    # return(get(eval(quote(rename))))
  }
  else{
    rm(e)
    assign(eval(object), get(object), envir = .GlobalEnv)
  }
}

