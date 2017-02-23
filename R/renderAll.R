#' Render all RMarkdown files in working directory
#' @export
#' @importFrom rmarkdown render
#' @param outputDir Output directory
#' @param ... Passthrough \code{rmarkdown::render()} parameters
renderAll <- function(outputDir = "render", ...) {
    # Create `outputDir` if necessary
    if (!file.exists(outputDir)) {
        dir.create(outputDir, recursive = TRUE)
    }
    sapply(
        list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE),
        function(a) {
            rmarkdown::render(a,
                              output_dir = outputDir,
                              output_format = "all",
                              clean = TRUE,
                              ...)
        }
    )
}
