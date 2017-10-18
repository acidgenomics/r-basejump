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
#'
#' @examples
#' readDataVersions(
#'     file.path(testDataURL, "data_versions.csv"))
NULL



# Methods ====
#' @rdname readDataVersions
#' @importFrom readr read_csv
#' @export
setMethod(
    "readDataVersions",
    signature("character"),
    function(object) {
        if (!file.exists(object)) {
            warning(paste(basename(object), "file missing"), call. = FALSE)
            return(NULL)
        }
        read_csv(object)
    })
