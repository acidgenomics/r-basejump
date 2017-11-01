#' Read Data Versions
#'
#' @rdname readDataVersions
#' @name readDataVersions
#' @family bcbio Utilities
#' @keywords internal
#'
#' @inheritParams saveData
#'
#' @param object Project directory path (character vector).
#'
#' @return [data.frame].
#'
#' @examples
#' readDataVersions("http://basejump.seq.cloud/data_versions.csv")
NULL



# Methods ====
#' @rdname readDataVersions
#' @importFrom readr read_csv
#' @export
setMethod(
    "readDataVersions",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        file <- .localOrRemoteFile(object, quiet = quiet)
        if (is.null(file)) {
            return(NULL)
        }
        read_csv(file, progress = FALSE)
    })
