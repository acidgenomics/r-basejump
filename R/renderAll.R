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
    # Get the recursive list of RMarkdown files
    files <- list.files(pattern = "\\.Rmd$",
                       full.names = TRUE,
                       recursive = TRUE) %>%
        # Ignore drafts
        .[!grepl("_draft\\.Rmd$", ., ignore.case = TRUE)]
    sapply(files, function(input) {
        rmarkdown::render(input,
                          output_dir = outputDir,
                          output_format = "all",
                          clean = TRUE,
                          ...)
    })
    invisible()
}
