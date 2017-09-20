#' Load Data File as Name
#'
#' @inheritParams loadData
#' @param mappings Named character vector to define mappings. The name defines
#'   the new name of the object in the environment, whereas the value in the
#'   vector denotes the original object name. This is designed to function
#'   like a key value pair.
#'
#' @return No value.
#' @export
#'
#' @examples
#' \dontrun{
#' loadDataAsName(c(foo = "mtcars", bar = "starwars"))
#' }
loadDataAsName <- function(mappings, dir = "data") {
    if (!is.character(mappings) |
        is.null(names(mappings))) {
        stop("mappings must be defined as a named character vector",
             call. = FALSE)
    }
    envir <- parent.frame()
    # Assign into a temporary environment rather than using `attach()`
    tmpenv <- new.env()
    lapply(seq_along(mappings), function(a) {
        object <- mappings[a]
        name <- names(object)
        # Check to see if full file path was passed
        fileExtPattern <- "\\.[A-Za-z0-9]+$"
        if (file.exists(object) &
            str_detect(object, fileExtPattern)) {
            file <- object
            # Extract the object name from the file name
            object <- basename(object) %>%
                str_replace(fileExtPattern, "")
        } else {
            file <- file.path(dir, paste0(object, ".rda"))
        }
        load(file, envir = tmpenv)
        assign(
            name,
            get(object, envir = tmpenv, inherits = FALSE),
            envir = envir)
    }) %>%
        invisible
}
