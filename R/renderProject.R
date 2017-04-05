#' Render all RMarkdown files in working directory
#'
#' @author Michael Steinbaugh
#'
#' @import rmarkdown
#'
#' @param outputDir Output directory
#' @param ... Passthrough \code{rmarkdown::render()} parameters
#'
#' @export
renderProject <- function(outputDir = NULL, ...) {
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
        .[!grepl("_draft\\.Rmd$", ., ignore.case = TRUE)] %>%
        # Ignore child chunk files
        .[!grepl("(footer|header)\\.Rmd$", .)]
    sapply(files, function(input) {
        rmarkdown::render(input,
                          output_dir = outputDir,
                          output_format = "all",
                          clean = TRUE,
                          ...)
    })
    invisible()
}
