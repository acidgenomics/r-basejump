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



# Methods ====
#' @rdname readDataVersions
#' @export
setMethod(
    "readDataVersions",
    signature = "character",
    definition = function(object) {
        if (!file.exists(object)) {
            warning(paste(basename(object), "missing"))
            return(NULL)
        }
        read_csv(object)
    })
