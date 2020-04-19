#' this sets the workig directory automatically for joint projects
#'
#' @param wd_h my working directory
#' @param wd_h2 my other directory
#' @param wd_alt alternative working directory
#' @param user_name name of the OS user as returned by sys.Info() 
#' @param user_name2 name of the OS user as returned by sys.Info() 
#' @return this function does not return anything
#' @export
#' @examples
#' detect_wd(wd_h = "C:/Users/Hanno/Dropbox/",
#'           wd_alt = "C:/Users/Hanno/",
#'           user_name = "Hanno")
#' 

detect_wd <- function(wd_h = "", 
                      wd_h2 = "",
                      wd_alt = "", 
                      user_name = "Hanno",
                      user_name2 = 'hah235') {
  
  # obtain user name, output message
  s <- Sys.info()
  print(paste0("User is ", s[names(s) == "user"], 
               ". WD will be set accordingly."))
  
  # set wd according to user name 
  if (s[names(s) == "user"] == user_name) {
    setwd(wd_h)
  } else if (s[names(s) == "user"] == user_name2) {
    setwd(wd_h2) 
  } else {
    setwd(wd_alt)
  }
}