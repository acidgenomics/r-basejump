#' Read Program Versions
#'
#' @rdname readProgramVersions
#' @name readProgramVersions
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param object Project directory path (character vector).
#'
#' @return [data.frame].
NULL



# Constructors ====
.readProgramVersions <- function(object) {
    file <- file.path(object, "programs.txt")
    if (!file.exists(file)) {
        warning(paste(basename(file), "missing"))
        return(NULL)
    }
    read_delim(file, col_names = c("program", "version"), delim = ",")
}



# Methods ====
#' @rdname readProgramVersions
#' @export
setMethod(
    "readProgramVersions",
    signature = "character",
    definition = .readProgramVersions)
