#' Find and Summarize Variables in a Data Frame
#'
#' @description
#' Searches for variables in a data frame based on a pattern and provides summary statistics.
#' If a pattern is provided, matches variables using exact matching first, then uses
#' Jaro-Winkler distance for fuzzy matching. Returns summary statistics including
#' variable type, missing value percentage, number of unique values, and numeric
#' summaries (min, max, mean) where applicable.
#'
#' @param data A data frame or tibble to search through
#' @param pattern Character string. Pattern to match variable names against. Empty pattern
#'   returns the first n variables (default: "")
#' @param n Integer. Maximum number of variables to return (default: 10)
#'
#' @return Invisibly returns a tibble with variable summaries and prints a formatted
#'   table to console. The table includes: Variable name, Type, Missing percentage,
#'   Unique value count, and numeric summaries (Min, Max, Mean) where applicable.
#'
#' @details
#' The function performs the following:
#' 1. For empty patterns, selects the first n variables
#' 2. For provided patterns:
#'    - First finds exact matches (case-insensitive)
#'    - If needed, adds fuzzy matches using Jaro-Winkler distance
#' 3. Generates summary statistics with proper formatting:
#'    - Missing values shown as percentages
#'    - Numeric summaries rounded to 2 decimal places
#'    - NA for non-applicable metrics (e.g., mean for character columns)
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
#'
find_variables <- function(data, pattern = "", n = 10) {
    # Validate inputs
    if (!is.data.frame(data)) {
        stop("`data` must be a data.frame or tibble.")
    }
    if (!is.character(pattern) || length(pattern) != 1) {
        stop("`pattern` must be a single string.")
    }
    if (!is.numeric(n) || n <= 0 || floor(n) != n) {
        stop("`n` must be a positive integer.")
    }

    # Select matching columns
    if (pattern == "") {
        matching_cols <- names(data)[1:min(n, length(names(data)))]
    } else {
        # First, identify variables that contain the pattern
        exact_matches <- names(data)[grepl(pattern, names(data), ignore.case = TRUE)]

        # If exact matches are found, prioritize them
        if (length(exact_matches) >= n) {
            matching_cols <- exact_matches[1:n]
        } else {
            # Calculate string distances for the remaining variables
            remaining_cols <- setdiff(names(data), exact_matches)
            distance_df <- tibble::tibble(
                column = remaining_cols,
                distance = stringdist::stringdist(tolower(column), tolower(pattern), method = "jw")
            ) %>%
                dplyr::arrange(distance) %>%
                dplyr::slice_head(n = n - length(exact_matches)) %>%
                dplyr::pull(column)

            # Combine exact matches with closest matches
            matching_cols <- c(exact_matches, distance_df)
        }
    }

    if (length(matching_cols) == 0) {
        message(glue::glue("No variables found matching pattern: '{pattern}'"))
        return(NULL)
    }

    # Summarize data with all metrics as character
    summary_df <- data %>%
        dplyr::select(dplyr::all_of(matching_cols)) %>%
        dplyr::summarise(dplyr::across(
            everything(),
            list(
                type = ~ as.character(class(.))[1],
                missing = ~ sprintf("%.2f%%", round(mean(is.na(.)) * 100, 2)),
                unique = ~ as.character(n_distinct(., na.rm = TRUE)),
                min = ~ if (is.numeric(.)) as.character(round(min(., na.rm = TRUE), 2)) else NA_character_,
                max = ~ if (is.numeric(.)) as.character(round(max(., na.rm = TRUE), 2)) else NA_character_,
                mean = ~ if (is.numeric(.)) as.character(round(mean(., na.rm = TRUE), 2)) else NA_character_
            )
        ), .groups = "drop") # Ensure grouping is dropped

    # Ensure all columns in summary_df are character
    summary_df <- summary_df %>%
        dplyr::mutate(dplyr::across(everything(), ~ as.character(.)))

    # Pivot longer with explicit transformation to character
    results_long <- summary_df %>%
        tidyr::pivot_longer(
            cols = everything(),
            names_to = c("variable", "metric"),
            names_sep = "_(?=[^_]+$)",
            values_to = "value",
            values_transform = list(value = as.character)
        )

    # Pivot wider to get the final summary table
    results_wide <- results_long %>%
        tidyr::pivot_wider(
            names_from = metric,
            values_from = value
        )

    # Print results with better formatting
    results_wide %>%
        knitr::kable(
            col.names = c("Variable", "Type", "Missing %", "Unique Values", "Min", "Max", "Mean"),
            align = c("l", "l", "r", "r", "r", "r", "r"), # Left-align text, right-align numbers
            caption = "Summary of Selected Variables"
        ) %>%
        print()

    invisible(results_wide)
}
