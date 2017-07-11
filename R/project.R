#' Project management utilities
#'
#' @rdname project
#' @description Create necessary directory structure for an
#'   [RMarkdown](http://rmarkdown.rstudio.com) report in
#'   [RStudio](https://www.rstudio.com).
#' @export
createProjectDirs <- function() {
    localDirs <- c("data", "figures", "meta", "results")
    lapply(seq_along(localDirs), function(a) {
        dir.create(localDirs[[a]], showWarnings = FALSE)
    }) %>%
        invisible
}



#' @rdname project
#' @description Detect if R is running on an HPC cluster.
#' @export
detectHPC <- function() {
    if (Sys.info()[["login"]] == "root" &
        Sys.info()[["sysname"]] == "Linux" &
        any(
            Sys.getenv("CDC_JOINED_DOMAIN") == "med.harvard.edu",
            Sys.getenv("LSB_EXEC_CLUSTER") == "hms_orchestra",
            grepl("\\.orchestra$", Sys.getenv("HOSTNAME")),
            grepl("\\.orchestra$", Sys.getenv("LSB_HOSTS")),
            grepl("@MED\\.HARVARD\\.EDU$", Sys.getenv("USER_PRINCIPAL_NAME"))
        )) {
        "orchestra"
    } else {
        FALSE
    }
}



#' @rdname project
#' @description Package project and build website.
#'
#' @param install Install package.
#'
#' @export
packageProject <- function(install = FALSE) {
    # Ensure package is up to date
    document()
    build_vignettes()
    load_all()

    # Run integrity checks
    BiocCheck::BiocCheck(getwd())
    check()

    # Save the package build to disk
    build()

    # Install the package
    if (isTRUE(install)) {
        install()
    }
}



#' @rdname project
#' @description Render all RMarkdown files in working directory.
#'
#' @param today Render to a subdirectory with today's date.
#'
#' @export
renderProject <- function(today = TRUE) {
    if (identical(getwd(), Sys.getenv("HOME"))) {
        stop("Working from HOME directory")
    }
    if (!length(dir(pattern = "*.Rproj"))) {
        warning("No Rproj file found")
    }

    if (isTRUE(today)) {
        outputDir <- file.path(Sys.Date())
        dir.create(outputDir, showWarnings = FALSE)
    } else {
        outputDir <- getwd()
    }

    # Get the list of RMarkdown files
    files <- list.files(pattern = "\\.Rmd$",
                        full.names = TRUE,
                        recursive = FALSE) %>%
        # Ignore drafts
        .[!grepl("_draft\\.Rmd$", ., ignore.case = TRUE)] %>%
        # Ignore child chunk files
        .[!grepl("(footer|header)\\.Rmd$", .)]
    if (!length(files)) {
        message("No RMarkdown files to render")
        return(invisible())
    }

    lapply(files, function(input) {
        render(input,
               clean = TRUE,
               envir = new.env(),
               knit_root_dir = getwd(),
               output_dir = outputDir,
               output_format = "all")
    }) %>%
        invisible
}
