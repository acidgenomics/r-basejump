#' Render all RMarkdown files in current project
#' @export
#' @importFrom rmarkdown render
#' @param dir Output directory.
#' @param ... Optional \code{rmarkdown} parameters
renderProject <- function(dir = "docs", ...) {
    sapply(
        list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE),
        function(a) {
            rmarkdown::render(a, output_dir = dir, output_format = "all", ...)
        }
    )
}
