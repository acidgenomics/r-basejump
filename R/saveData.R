# This function fails with `rmarkdown::render()`. Likely need to change
# environment settings for getting objects from dots. Not exported until
# this issue is fixed.

#' Save data files
#'
#' This function makes it easy to save data in the correct format. Based off
#' \code{use_data()} from the \code{devtools} package. Note that this function
#' will overwrite existing data files.
# @export
#' @param ... Unquoted names of existing objects to save
#' @param dir Target directory
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- 1:100
#' saveData(x, y)
#' }
saveData <- function(..., dir = "data") {
    if (!isString(dir)) {
        stop("dir must be a string")
    }
    if (!file.exists(dir)) {
        dir.create(dir)
    }
    objs <- get_objs_from_dots(dots(...))
    paths <- file.path(dir, paste0(objs, ".rda"))
    message("Saving ", paste(unlist(objs), collapse = ", "),
            " as ", paste(basename(paths), collapse = ", "))
    mapply(save, list = objs, file = paths)
    invisible()
}
