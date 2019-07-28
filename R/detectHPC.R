#' Detect HPC environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @note Currently supports detection of Slurm or LSF.
#' @note Updated 2019-07-28.
#' @export
#'
#' @return `character(1)` or `logical(1)`.
#'   Workload manager (scheduler) name if HPC is detected (e.g. `"SLURM"` or
#'   `"LSF"`), otherwise `FALSE`.
#'
#' @seealso
#' - `Sys.getenv`.
#' - `Sys.info`.
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
