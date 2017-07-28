#' Assign Multiple Objects to a New Environment
#'
#' @param ... Dot objects to assign.
#' @param envirName Environment name.
#'
#' @return Object names.
#' @export
#'
#' @examples
#' assignAsNewEnv(mtcars, starwars, envirName = "testenv")
assignAsNewEnv <- function(..., envirName) {
    if (!is_string(envirName)) {
        stop("Environment name must be a string")
    }
    envir <- new.env()
    dots <- dots(...)
    objs <- getObjsFromDots(dots)
    lapply(seq_along(objs), function(a) {
        assign(objs[a], dots[[a]], envir = envir)
    }
    ) %>% invisible
    message(paste("Assigning", toString(objs), "to", envirName))
    assign(envirName, envir, parent.frame())
    objs
}



#' Clear Warnings
#'
#' @keywords internal
#'
#' @return No value.
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}



#' Create Project Directory Structure
#'
#' Create necessary directory structure for an
#' [RMarkdown](http://rmarkdown.rstudio.com) report in
#' [RStudio](https://www.rstudio.com).
#'
#' @return No value.
#' @export
createProjectDirs <- function() {
    localDirs <- c("data", "figures", "meta", "results")
    lapply(seq_along(localDirs), function(a) {
        dir.create(localDirs[[a]], showWarnings = FALSE)
    }) %>%
        invisible
}



#' Detect HPC Environment
#'
#' Detect if R is running on an HPC cluster.
#'
#' @return [logical].
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



#' Ensembl Build Version
#'
#' @keywords internal
#'
#' @return String containing Ensembl build version of annotables.
#' @export
ensemblVersion <- function() {
    annotables::ensembl_version
}
