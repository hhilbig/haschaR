## Summarize the Output from rdrobust
## (works similar to tidy() for lm objects)

#' @title Summarize the Output from rdrobust
#' @param model
#' @param se_nr specify the standard error: 1 = conventional, 2 = Bias-Corrected, 3 = Robust
#' @export
#' @examples
#'
#'

tidy_rd <- function(model, se_nr){

  est <- model$coef[se_nr]
  se  <- model$se[se_nr]

  low  <- model$ci[se_nr, 1]
  high <- model$ci[se_nr, 2]

  zstat <- model$z[se_nr]
  pval <- model$pv[se_nr]

  h_left <- model$bws[1,1]
  h_right <- model$bws[1,2]

  b_left <- model$bws[2,1]
  b_right <- model$bws[2,2]

  p <- model$p
  n <- sum(model$N)

  smry <- tibble(estimate = est,
                 std.error = se,
                 statistic = zstat,
                 p.value = pval,
                 conf.low = low,
                 conf.high = high,
                 p = p,
                 n = n,
                 bw_left_h = h_left,
                 bw_right_h = h_right,
                 bw_left_b = b_left,
                 bw_right_b = b_right)

  return(smry)

}

