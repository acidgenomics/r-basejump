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
