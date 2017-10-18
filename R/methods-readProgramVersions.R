#' Read Program Versions
#'
#' @rdname readProgramVersions
#' @name readProgramVersions
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
#' readProgramVersions(file.path(testDataURL, "programs.txt"))
NULL



# Methods ====
#' @rdname readProgramVersions
#' @importFrom readr read_delim
#' @export
setMethod(
    "readProgramVersions",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        file <- .localOrRemoteFile(object, quiet = quiet)
        # programs.txt, but is comma separated
        read_csv(
            file,
            col_names = c("program", "version"),
            progress = FALSE)
    })
