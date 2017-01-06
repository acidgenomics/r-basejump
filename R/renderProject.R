#' Render all RMarkdown files in current project
#'
#' @import rmarkdown
#'
#' @param dir Output directory.
#' @param ... Optional rmarkdown parameters
#'
#' @export
renderProject <- function(dir = "docs", ...) {
    sapply(
        list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE),
        function(a) {
            rmarkdown::render(a, output_dir = dir, output_format = "all", ...)
        }
    )
}
