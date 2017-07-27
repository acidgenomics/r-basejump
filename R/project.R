#' Project Management Utilities
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
