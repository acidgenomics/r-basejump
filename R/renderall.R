#' Render all RMarkdown files
#'
#' @import rmarkdown
#'
#' @param ... Optional rmarkdown parameters
#'
#' @export
#'
#' @examples
#' renderall(output_dir = "docs")
renderall <- function(...) {
    sapply(
        list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE),
        function(a) {
            rmarkdown::render(a, output_format = "all", ...)
        }
    )
}
