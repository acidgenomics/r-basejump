#' Read Program Versions
#'
#' @family bcbio Project Directory File Utilities
#' @keywords internal
#'
#' @param projectDir Project directory.
#'
#' @return [data.frame].
#' @export
.programs <- function(projectDir) {
    file <- file.path(projectDir, "programs.txt")
    if (!file.exists(file)) {
        warning(paste(basename(file), "missing"))
        return(NULL)
    }
    read_delim(file, col_names = c("program", "version"), delim = ",")
}
