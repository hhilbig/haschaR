## Return residuals from a fixed effects model
#' @title Return residuals from a fixed effects model
#' @param y character, dependent variable
#' @param rhs_continuous character vector, right-hand side continuous variables
#' @param rhs_fe character vector, right-hand side fixed effects (basically any categorical variable)
#' @param data data.frame, data
#' @export

feols_residualize <- function(
    y,
    rhs_continuous,
    rhs_fe,
    data) {
    ## Make the formula

    fmla <- paste0(
        y, " ~1",
        ifelse(!rhs_continuous == "",
            paste0(
                "+",
                paste0(rhs_continuous, collapse = " + ")
            ),
            ""
        ), " | ", paste0(rhs_fe, collapse = " + ")
    ) %>%
        as.formula()

    ## Do

    m_res <- feols(fmla, data = data, cluster = ~ags)

    ## Get residuals

    res_y <- residuals(m_res)

    ## Return

    return(res_y)
}

