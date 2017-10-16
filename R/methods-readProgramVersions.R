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
NULL



# Methods ====
#' @rdname readProgramVersions
#' @importFrom readr read_delim
#' @export
setMethod(
    "readProgramVersions",
    signature("character"),
    function(object) {
        if (!file.exists(object)) {
            warning(paste(basename(object), "file missing"), call. = FALSE)
            return(NULL)
        }
        read_delim(
            object,
            col_names = c("program", "version"),
            delim = ",")
    })
