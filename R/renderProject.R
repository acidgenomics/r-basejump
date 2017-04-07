#' Render all RMarkdown files in working directory
#'
#' @author Michael Steinbaugh
#'
#' @import rmarkdown
#'
#' @param outputDir Output directory
#' @param recursive Find files recursively
#'
#' @export
renderProject <- function(
    ouputDir = "docs",
    recursive = FALSE) {
    if (!length(dir(pattern = "*.Rproj"))) {
        stop("no Rproj file found")
    }
    if (identical(getwd(), Sys.getenv("HOME"))) {
        stop("careful, working from HOME")
    }

    # Create the output directory
    dir.create(outputDir, showWarnings = FALSE)

    # Get the list of RMarkdown files
    files <- list.files(pattern = "\\.Rmd$",
                        full.names = TRUE,
                        recursive = recursive) %>%
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
    }) %>% invisible
}
