#' Render all RMarkdown files in working directory
#'
#' @author Michael Steinbaugh
#'
#' @import rmarkdown
#'
#' @param outputDir Output directory
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
                          clean = TRUE,
                          envir = new.env(),
                          knit_root_dir = getwd(),
                          output_dir = outputDir,
                          output_format = "all")
    })
    invisible()
}
