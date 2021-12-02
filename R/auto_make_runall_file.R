#' This makes a runall file from a directory that has r files 
#'
#' @param code_directory Directory that has all the code (needs full name)
#' @param out_file outout .R file
#' @param send_to_clipboard if T, then output will be sent to clipboard
#' @return This saves an .R file
#' @import stringr tidyverse clipr
#' @export
#' @examples
#' auto_make_runall_file(code_directory = "C:/Users/hanno/Dropbox/Harvard/Projects/Newspapers/02_Code/", 
#'                      out_file = "02_Code/RUNALL_TEST.R")
#'
#' 

auto_make_runall_file <- function(code_directory, out_file = '',
                                  send_to_clipboard = T) {
  
  message('script ignores anything with the terms OLD|Old|old|Archive|ARCHIVE')
  message('Files not in any subfolder are at the end of the output file')
  
  ## Get R files
  
  rfiles <- code_directory %>% 
    dir(full.names = T, recursive = T) %>% 
    haschaR::str_filter('.R') %>% 
    str_remove_all(code_directory) %>% 
    str_filter('OLD|Old|old|Archive|ARCHIVE', neg = T) %>% 
    ifelse(substr(., 1, 1) == '/', substr(., 2, 1000), .) %>% 
    str_split('/') %>% 
    lapply(function(x) {
      l = length(x)
      
      if (l == 1) {
        
        gname = 'Top level'
        
      } else {
        
        gname = paste0(x[-l], collapse = '/')
        
      }
      
      ## Return
      
      list('file' = paste0(x, collapse = '/'), 
           'group' = gname)
    })
  
  ## Make a grouped list
  
  glist <- rfiles %>% sapply(function(x) x$group) %>% unique()
  
  glist <- glist %>% 
    lapply(function(g) {
      
      flist <- sapply(rfiles, function(f) if(f$group == g) f$file) %>% 
        unlist()
      
      ## Return
      
      list('group' = g, 'files' = flist)
      
      
    })
  
  ## Make function to write text document
  
  gname <- glist[[1]]$group
  gfiles <- glist[[1]]$files
  
  write_one_group <- function(gname, gfiles) {
    
    g <- c(paste0('## ', str_replace_all(gname, '_', ' ')), '', 
           paste0("source(\"", paste0(code_directory, gfiles), "\")"), '')
    g
    
  }
  
  ## Do
  
  file_text <- glist %>% 
    sapply(function(x) write_one_group(x$group, x$files)) %>% 
    unlist()
  
  if (!out_file == '') {
    
    cat('saving to ', out_file)
    
    ## Write all to file
    
    fileConn <- file(out_file)
    writeLines(file_text, fileConn)
    close(fileConn)
    
  } else {
    
    if (send_to_clipboard) {
      
      write_clip(file_text)
      
    } else {
      
      cat(file_text, sep= '\n')
      
    }
    
  }
  
}

