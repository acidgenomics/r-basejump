#' Render all RMarkdown files in working directory
#'
#' @author Michael Steinbaugh
#'
#' @param outputDir Output directory
#' @param recursive Find files recursively
#'
#' @export
#'
#' @examples
#' \dontrun{
#' renderProject()
#' }
renderProject <- function(
    outputDir = file.path("docs", Sys.Date()),
    recursive = FALSE) {
    if (!length(dir(pattern = "*.Rproj"))) {
        stop("no Rproj file found")
    }
    if (identical(getwd(), Sys.getenv("HOME"))) {
        stop("careful, working from HOME")
    }

    # Create the output directory
    dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

    # Get the list of RMarkdown files
    files <- list.files(pattern = "\\.Rmd$",
                        full.names = TRUE,
                        recursive = recursive) %>%
        # Ignore drafts
        .[!grepl("_draft\\.Rmd$", ., ignore.case = TRUE)] %>%
        # Ignore child chunk files
        .[!grepl("(footer|header)\\.Rmd$", .)]

    sapply(files, function(input) {
        render(input,
               clean = TRUE,
               envir = new.env(),
               knit_root_dir = getwd(),
               output_dir = outputDir,
               output_format = "all")
    }) %>% invisible
}
