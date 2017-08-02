#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @note Currently supports detection of
#'   [HMS Orchestra](https://rc.hms.harvard.edu/#orchestra).
#'
#' @rdname detectHPC
#'
#' @return [logical].
#' @export
#'
#' @seealso
#' - `Sys.info()`.
#' - `R.version`.
#' - `.Platform`.
#'
#' @examples
#' detectHPC()
setMethod("detectHPC", "missing", function() {
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
})
