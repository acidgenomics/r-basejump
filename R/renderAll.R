#' Render all RMarkdown files in working directory
#' @export
#' @importFrom rmarkdown render
#' @param outputDir Output directory
#' @param ... Passthrough \code{rmarkdown::render()} parameters
renderAll <- function(outputDir = NULL, ...) {
    if (!is.null(outputDir)) {
        # Create `outputDir` if necessary
        if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE)
        }
    }
    sapply(
        # Match `.Rmd` files, except drafts
        list.files(pattern = "[^(_draft)]\\.Rmd$",
                   full.names = TRUE, recursive = TRUE),
        function(input) {
            rmarkdown::render(input,
                              output_dir = outputDir,
                              output_format = "all",
                              clean = TRUE,
                              ...)
        }
    )
    # invisible()
}
