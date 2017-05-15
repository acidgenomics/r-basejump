#' R project management utilities.
#'
#' @rdname project
#' @aliases package_proj
#' @description Build package and website.
#'
#' @param install Install package.
#' @param test Run tests with [devtools::test()].
#'
#' @export
packageProj <- function(
    install = FALSE,
    test = FALSE) {
    # Ensure package is up to date
    devtools::document()
    devtools::build_vignettes()
    devtools::load_all()

    # Run integrity checks
    BiocCheck(getwd())
    devtools::check()
    if (isTRUE(test)) {
        devtools::test()
    }

    # Save the build to disk
    devtools::build()

    # Install the package
    if (isTRUE(install)) {
        devtools::install()
    }

    # Ensure safe developer environment
    biocValid()
}



#' @rdname project
#' @aliases render_proj
#' @description Render all RMarkdown files in working directory.
#'
#' @param outputDir Output directory.
#' @param recursive Find files recursively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' renderProj()
#' }
renderProj <- function(
    outputDir = file.path("docs", Sys.Date()),
    recursive = FALSE) {
    if (!length(dir(pattern = "*.Rproj"))) {
        warning("no Rproj file found")
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



#' @rdname project
#' @aliases clear_warnings
#' @description Clear warnings.
#' @export
#'
#' @examples
#' \dontrun{
#' clearWarnings()
#' }
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
