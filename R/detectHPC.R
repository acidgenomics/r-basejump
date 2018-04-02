#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @note Currently supports detection of
#'   [HMS Orchestra](https://rc.hms.harvard.edu/#orchestra).
#'
#' @return Scheduler name if HPC is detected (e.g. LSF, SLURM), otherwise
#'   `FALSE`.
#' @export
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
