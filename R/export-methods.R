#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for [rio::export()] that adds support for additional S4
#' classes in Bioconductor.
#'
#' @name export
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [rio::export()].
#'
#' @examples
#' export(mtcars, format = "csv")
NULL



#' @rdname export
#' @export
setMethod(
    "export",
    signature("ANY"),
    function(x, file, format, ...) {
        call <- matchCall()
        name <- call[["x"]]
        if (missing(file)) {
            assert_is_a_string(format)
            file <- paste0(name, ".", format)
        }
        message(paste("Exporting", name, "to", file))
        rio::export(x = x, file = file, ...)
    }
)
