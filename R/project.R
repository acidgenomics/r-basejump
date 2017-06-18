#' Project management utilities
#'
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
    }
    ) %>% invisible
}

#' @rdname aliases
#' @usage NULL
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

#' @rdname aliases
#' @usage NULL
#' @export
detect_hpc <- detectHPC



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

    # Build website
    build_site()
}

#' @rdname aliases
#' @usage NULL
#' @export
package_project <- packageProject



#' @rdname project
#' @description Render all RMarkdown files in working directory.
#'
#' @param output_dir Output directory.
#' @param recursive Find files recursively.
#'
#' @export
renderProject <- function(
    output_dir = file.path("docs", Sys.Date()),
    recursive = FALSE) {
    if (!length(dir(pattern = "*.Rproj"))) {
        warning("no Rproj file found")
    }
    if (identical(getwd(), Sys.getenv("HOME"))) {
        stop("careful, working from HOME")
    }

    # Create the output directory
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
               output_dir = output_dir,
               output_format = "all")
    }
    ) %>% invisible
}

#' @rdname aliases
#' @usage NULL
#' @export
render_project <- renderProject



#' @rdname project
#' @description Clear warnings.
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}

#' @rdname aliases
#' @usage NULL
#' @export
clear_warnings <- clearWarnings
