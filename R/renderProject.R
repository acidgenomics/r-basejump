#' Render all RMarkdown files in working directory
#'
#' @author Michael Steinbaugh
#'
#' @import rmarkdown
#'
#' @export
renderProject <- function(ouputDir = NULL) {
    if (identical(getwd(), Sys.getenv("HOME"))) {
        stop("careful, working from HOME")
    }
    if (!length(dir(pattern = "*.Rproj"))) {
        stop("no Rproj file found")
    }
    
    # Output to docs in a dated subfolder by default
    if (is.null(outputDir)) {
        outputDir <- file.path("docs", Sys.Date())
    }
    
    dir.create(outputDir, showWarnings = FALSE)
    
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
    }) %>% invisible
}
