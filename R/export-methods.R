#' Export
#'
#' Export data out of R and write to disk.
#'
#' @name export
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [rio::export()].
NULL



#' @rdname export
#' @export
setMethod(
    "export",
    signature("ANY"),
    function(object, format = "csv", dir = ".") {
        call <- matchCall()
        assert_is_a_string(format)
        dir <- initializeDirectory(dir)
        name <- call[["object"]]
        file <- file.path(dir, paste0(name, ".", format))
        message(paste("Exporting", name, "to", file))
        rio::export(x = object, file = file)
    }
)
