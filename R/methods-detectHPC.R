#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @rdname detectHPC
#' @name detectHPC
#'
#' @note Currently supports detection of
#'   [HMS Orchestra](https://rc.hms.harvard.edu/#orchestra).
#'
#' @return Scheduler name if `TRUE` (e.g. LSF, SLURM), otherwise `FALSE`.
#'
#' @seealso
#' - `Sys.getenv()`.
#' - `Sys.info()`.
#' - `R.version`.
#' - `.Platform`.
#'
#' @examples
#' detectHPC()
NULL



# Methods ====
#' @rdname detectHPC
#' @export
setMethod("detectHPC", "missing", function() {
    if (Sys.getenv("LSF_ENVDIR") != "") {
        "LSF"
    } else if (Sys.getenv("SLURM_CONF") != "") {
        "SLURM"
    } else {
        FALSE
    }
})
