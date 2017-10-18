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
#' readDataVersions(file.path(testDataURL, "data_versions.csv"))
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
        read_csv(
            file,
            # c = character; T = datetime
            col_types = "ccT",
            progress = FALSE)
    })
