#' Project management utilities
#'
#' @rdname project
#'
#' @param install Install package.
#' @param test Run tests with [devtools::test()].



#' @rdname project
#' @description Create necessary directory structure for an
#'   [RMarkdown](http://rmarkdown.rstudio.com) report in
#'   [RStudio](https://www.rstudio.com).
#' @export
createProjectDirs <- function() {
    local_dirs <- c("data",
                    "figures",
                    "meta",
                    "results")
    lapply(seq_along(local_dirs), function(a) {
        dir.create(local_dirs[a], showWarnings = FALSE)
    }) %>% invisible
}

#' @rdname project
#' @export
create_project_dirs <- createProjectDirs



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
#' @export
detect_hpc <- detectHPC



#' @rdname project
#' @description Package project and build website.
#' @export
packageProject <- function(
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
#' @export
package_project <- packageProject



#' @rdname project
#' @description Render all RMarkdown files in working directory.
#'
#' @param outputDir Output directory.
#' @param recursive Find files recursively.
#'
#' @export
renderProject <- function(
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
#' @export
render_project <- renderProject



#' @rdname project
#' @description Clear warnings.
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}

#' @rdname project
#' @export
clear_warnings <- clearWarnings
