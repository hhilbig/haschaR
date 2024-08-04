#' Remove unused plot files
#'
#' This function identifies and removes PDF plots in the specified plot directory that are not referenced by any R scripts in the specified code directory.
#'
#' @param code_dir The directory containing the R code files.
#' @param plot_dir The directory containing the PDF plot files.
#' @return A character vector of the plots that were deleted.
#' @export
#' @examples
#' remove_unused_plots(
#'     code_dir = "path/to/code/directory",
#'     plot_dir = "path/to/plot/directory"
#' )
#'
rremove_unused_plots <- function(code_dir, plot_dir) {
    # Load necessary libraries
    library(tidyverse)

    # List all PDF files in the plot directory
    plot_files <- list.files(plot_dir, pattern = "\\.pdf$", full.names = TRUE)
    plot_names <- basename(plot_files)

    # Read all R files in the code directory
    r_files <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
    r_content <- lapply(r_files, readLines)

    # Check if plot names appear in any of the R files
    used_plots <- sapply(plot_names, function(plot) {
        any(sapply(r_content, function(content) {
            any(grepl(plot, content))
        }))
    })

    # Identify unused plots
    unused_plots <- plot_files[!used_plots]

    # Delete unused plots
    sapply(unused_plots, file.remove)

    # Output results
    cat("The following plots were deleted:\n")
    cat(unused_plots, sep = "\n")
}
