#' Find and Summarize Variables in a Data Frame
#'
#' @description
#' Searches for variables in a data frame based on a pattern and provides summary statistics
#' for the matching variables.
#'
#' @param data A data frame or tibble to search through
#' @param pattern Character string. Pattern to match variable names against (default: "")
#' @param n Integer. Maximum number of variables to return (default: 10)
#'
#' @return Invisibly returns a tibble with variable summaries. Prints a formatted table to console.
#'
#' @details
#' When pattern is empty, returns the first n variables. When pattern is provided,
#' uses Jaro-Winkler distance to find closest matching variable names.
#'
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr select summarise across everything arrange slice_head pull
#' @importFrom stringr str_glue
#' @importFrom stringdist stringdist
#' @importFrom knitr kable
#'
#' @examples
#' \dontrun{
#' find_variables(mtcars)
#' find_variables(mtcars, "cyl", n = 5)
#' }
#'
#' @export
find_variables <- function(data, pattern = "", n = 10) {
    # If pattern is empty, return all variables (limited to n)
    if (pattern == "") {
        matching_cols <- names(data)[1:min(n, length(names(data)))]
    } else {
        # Calculate string distances and rank columns
        matching_cols <- tibble(
            column = names(data),
            distance = stringdist::stringdist(tolower(column), tolower(pattern), method = "jw")
        ) %>%
            arrange(distance) %>%
            slice_head(n = n) %>%
            pull(column)
    }

    if (length(matching_cols) == 0) {
        message(str_glue("No variables found matching pattern: '{pattern}'"))
        return(NULL)
    }

    # Create base summary
    results <- data %>%
        select(all_of(matching_cols)) %>%
        summarise(across(
            everything(),
            list(
                type = ~ class(.)[1],
                missing = ~ round(mean(is.na(.)) * 100, 2),
                unique = ~ n_distinct(., na.rm = TRUE),
                min = ~ if (is.numeric(.)) round(min(., na.rm = TRUE), 2) else NA_real_,
                max = ~ if (is.numeric(.)) round(max(., na.rm = TRUE), 2) else NA_real_,
                mean = ~ if (is.numeric(.)) round(mean(., na.rm = TRUE), 2) else NA_real_
            )
        )) %>%
        pivot_longer(
            everything(),
            names_to = c("variable", "metric"),
            names_sep = "_(?=[^_]+$)"
        ) %>%
        pivot_wider(
            names_from = metric,
            values_from = value
        )

    # Print results nicely
    results %>%
        knitr::kable(
            col.names = c("Variable", "Type", "Missing %", "Unique Values", "Min", "Max", "Mean"),
            align = "l"
        ) %>%
        print()

    invisible(results)
}
