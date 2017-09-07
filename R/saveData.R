#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @rdname saveData
#' @name saveData
#' @family Write Utilities
#'
#' @inheritParams base::save
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to **data**.
#'
#' @note These function will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [usethis::use_data()].
#' - [devtools::use_data()].
#' - [base::save()].
#'
#' @return No value.
#' @export
#'
#' @examples
#' saveData(mtcars, starwars)
saveData <- function(...,  dir = "data", compress = TRUE) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    names <- dots(..., character = TRUE)
    paths <- file.path(dir, paste0(names, ".rda"))
    message(paste("Saving", toString(names), "to", dir))
    mapply(save,
           list = names,
           file = paths,
           MoreArgs = list(envir = parent.frame(),
                           compress = compress))
    invisible()
}
