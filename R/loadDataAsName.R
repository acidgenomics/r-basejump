#' Load Data File as Name
#'
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
    envir <- parent.frame()
    # Assign into a temporary environment rather than using `attach()`
    tmpenv <- new.env()
    lapply(seq_along(mappings), function(a) {
        object <- mappings[a]
        name <- names(object)
        # Check to see if full file path was passed
        if (file.exists(object) &
            str_detect("\\.[A-Za-z0-9]+$")) {
            file <- object
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
