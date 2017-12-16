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
#' url <- file.path(
#'     "http://basejump.seq.cloud",
#'     "bcbio",
#'     "programs.txt")
#' readProgramVersions(url)
NULL



# Methods ======================================================================
#' @rdname readProgramVersions
#' @importFrom readr read_csv
#' @export
setMethod(
    "readProgramVersions",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        file <- .localOrRemoteFile(object, quiet = quiet)
        if (is.null(file)) {
            return(invisible())
        }
        # programs.txt, but is comma separated
        read_csv(
            file,
            col_names = c("program", "version"),
            # `c` denotes character here
            col_types = "cc",
            progress = FALSE)
    })
