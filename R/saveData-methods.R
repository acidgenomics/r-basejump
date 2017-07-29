#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @rdname saveData
#'
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to **data**.
#' @param compress Compression method, supporting `xz` (**preferred**), `bzip2`,
#'   or `gzip`. Compression can be disabled by setting as `FALSE`, although this
#'   is not generally recommended.
#'
#' @note These function will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [base::save()]
#' - [devtools::use_data()].
#' - https://github.com/hadley/devtools/blob/master/R/infrastructure.R
#'
#' @return No value.
#' @export
#'
#' @examples
#' saveData(mtcars, starwars)
setMethod(
    "saveData",
    signature("..." = "ANY"),
    function(..., dir, compress) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
        names <- dots(..., character = TRUE)
        paths <- file.path(dir, paste0(names, ".rda"))
        message(paste("Saving", toString(names), "to", dir))
        mapply(save, list = names, file = paths, compress = compress)
        invisible()
    })
