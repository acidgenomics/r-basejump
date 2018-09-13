#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @note Currently supports detection of
#'   [HMS Orchestra](https://rc.hms.harvard.edu/#orchestra).
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @return Scheduler name `string` if HPC is detected (e.g. LSF, SLURM),
#'   otherwise `FALSE`.
#'
#' @seealso
#' - `Sys.getenv()`.
#' - `Sys.info()`.
#' - `R.version`.
#' - `.Platform`.
#'
#' @examples
#' detectHPC()
detectHPC <- function() {
    if (!identical(Sys.getenv("LSF_ENVDIR"), "")) {
        "LSF"
    } else if (!identical(Sys.getenv("SLURM_CONF"), "")) {
        "SLURM"
    } else {
        FALSE
    }
}
