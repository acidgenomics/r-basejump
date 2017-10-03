#' Read Data Versions
#'
#' @family bcbio Project Directory File Utilities
#' @keywords internal
#'
#' @param projectDir Project directory.
#'
#' @return [data.frame].
#' @export
.dataVersions <- function(projectDir) {
    file <- file.path(projectDir, "data_versions.csv")
    if (!file.exists(file)) {
        warning(paste(basename(file), "missing"))
        return(NULL)
    }
    read_csv(file)
}
