#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @note Currently supports detection of Slurm or LSF.
#'
#' @export
#'
#' @return `string` or `boolean`. Workload manager (scheduler) name if HPC is
#'   detected (e.g. `"SLURM"` or `"LSF"`), otherwise `FALSE`.
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
