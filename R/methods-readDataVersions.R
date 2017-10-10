#' Read Data Versions
#'
#' @rdname readDataVersions
#' @name readDataVersions
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param object Project directory path (character vector).
#'
#' @return [data.frame].
NULL



# Constructors ====
.readDataVersions <- function(object) {
    file <- file.path(object, "data_versions.csv")
    if (!file.exists(file)) {
        warning(paste(basename(file), "missing"))
        return(NULL)
    }
    read_csv(file)
}



# Methods ====
#' @rdname readDataVersions
#' @export
setMethod(
    "readDataVersions",
    signature = "character",
    definition = .readDataVersions)
