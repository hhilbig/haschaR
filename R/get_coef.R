#' this extracts the coefficients from lm 
#'
#' @param model lm object
#' @param which.ivar which independet variable should be obtained?
#' @return a vector of estimation results
#' @export
#' @examples
#' get_coefs(model = lm(Petal.Length ~ Petal.Width, data = iris), which.ivar = "Petal.Width")
#' 
## cofficient extracton function


get_coef <- function(model = m1, which.ivar = "Petal.Width") {
  
  ## warnings
  
  if (nchar(which.ivar) == 0) stop("specify independent variable")
  if (!(which.ivar %in% names(coef(model)))) stop("independent variable not found")
  
  ## actual stuff
  
  s <- summary(model) # summary
  pos_coef <- which(row.names(s$coefficients) == which.ivar) # find variable position
  coef <- s$coefficients[pos_coef, 1] # find coef
  se <- s$coefficients[pos_coef, 2] # find se
  p_val <- s$coefficients[pos_coef, 4] # pvalue
  rsq <- s$r.squared # r square
  n <- s$df[2] + s$df[1] # = (n-p) + p
  
  c("coef" = coef, "se" = se, "pval" = p_val,
    "n" = n, "r2" = rsq) # output
}



