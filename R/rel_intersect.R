#' Checks the relative size of the intersection of two sets
#'
#' @param data1 First data set
#' @param data2 Second data set
#' @param varname1 Name of variable in first data set
#' @param varname2 Name of variable in second data set
#' @return This returns some overlap statistics
#' @export

rel_intersect <- function(data1, data2, varname1, varname2) {
  
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  v1 <- data.frame(data1)[, varname1]
  v2 <- data.frame(data2)[, varname2]
  
  ## Get relative frequency of missings
  
  rel_miss1 <- sum(is.na(v1)) / n1
  rel_miss2 <- sum(is.na(v2)) / n2
  
  ## Only valid obs
  
  v1_valid <- v1[!is.na(v1)]
  v2_valid <- v2[!is.na(v2)]
  
  ## Get intersection
  
  is1_2 <- sum(v1_valid %in% v2_valid, 
               na.rm = T) / n1
  is2_1 <- sum(v2_valid %in% v1_valid, 
               na.rm = T) / n2
  
  ## Prep for output
  
  cnames <- c('N', '% Missing', '% Valid', '% Overlap',
              '% overlap, of valid')
  row1 <- c(n1, rel_miss1, 
            1 - rel_miss1, 
            is1_2, is1_2 / (1 - rel_miss1))
  row2 <- c(n2, rel_miss2, 
            1 - rel_miss2, 
            is2_1, is2_1 / (1 - rel_miss2))
  
  ## Print
  
  out <- data.frame(rbind(row1, row2))
  colnames(out) <- cnames
  rownames(out) <- c('DF1', 'DF2')
  print(format(out, justify = 'left', digits = 3))
  invisible(out)
}
