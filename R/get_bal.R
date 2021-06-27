#' Standardized Covariate Balance
#'
#' @param treatvar treatment variable
#' @param cov_list list of covariates to assess balance on
#' @param FE fixed effects: stratify by this variable when assessing balance
#' @param data.df data frame storing all variables
#' @export
#' @return This function returns the standardized covariate balance

get_bal <- function(treatvar, cov_list, data.df, FE) {

  out_temp <- lapply(cov_list, function(cv) {

    if(length(FE) == 0){
      f <- as.formula(paste0(cv, ' ~', treatvar))
    }

    else{
      f <- as.formula(paste0(cv, ' ~', treatvar, '+', FE))
      }

    ## standardize covariates
    data.df[,cov_list] <- scale(data.df[,cov_list])

    ## model
    m <- lm(f, data = data.df)

    ## get coefs
    coef_list <- summary(m)$coefficients[2, 1]
    lower_list <- coef_list - 1.96*summary(m)$coefficients[2, 2]
    upper_list <- coef_list + 1.96*summary(m)$coefficients[2, 2]

    ##
    data.frame(cov = cv, lower = lower_list, upper = upper_list,
               coef = coef_list, tv = treatvar, stringsAsFactors = F)
  })

  do.call('rbind', out_temp)
}



