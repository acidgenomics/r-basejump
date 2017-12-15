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
#' url <- file.path(
#'     "http://basejump.seq.cloud",
#'     "bcbio",
#'     "data_versions.csv")
#' readDataVersions(url)
NULL



# Methods ======================================================================
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
            return(invisible())
        }
        read_csv(file, progress = FALSE)
    })
