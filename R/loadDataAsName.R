#' Load Data File as Name
#'
#' @inheritParams loadData
#'
#' @param mappings Named character vector to define mappings. The name defines
#'   the new name of the object in the environment, whereas the value in the
#'   vector denotes the original object name. This is designed to function
#'   like a key value pair.
#' @param envir The environment where the data should be loaded.
#'
#' @return Silently return named character vector of file paths.
#' @export
#'
#' @examples
#' # Use a named character vector as key value pair
#' \dontrun{
#' loadDataAsName(c(foo = "mtcars", bar = "starwars"))
#' }
loadDataAsName <- function(
    mappings,
    dir = "data",
    envir = parent.frame()) {
    if (!is.character(mappings) |
        is.null(names(mappings))) {
        stop("'mappings' must be defined as a named character vector",
             call. = FALSE)
    }
    if (!is.environment(envir)) {
        stop("'envir' must be an environment", call. = FALSE)
    }
    # Assign into a temporary environment, rather than using `attach()`
    tmpenv <- new.env()
    loaded <- sapply(seq_along(mappings), function(a) {
        object <- mappings[a]
        name <- names(object)
        # Check to see if full file path was passed
        fileExtPattern <- "\\.[A-Za-z0-9]+$"
        if (grepl(x = object, pattern = fileExtPattern)) {
            file <- object
            # Extract the object name from the file name
            object <- gsub(
                x = basename(object),
                pattern = fileExtPattern,
                replacement = "")
        } else {
            file <- file.path(dir, paste0(object, ".rda"))
        }
        if (!file.exists(file)) {
            stop(paste(object, "missing"), call. = FALSE)
        }
        file <- normalizePath(file)
        loaded <- load(file, envir = tmpenv)
        if (!identical(as.character(object), loaded)) {
            stop(paste(
                name, "file and saved object names are not identical"
            ), call. = FALSE)
        }
        assign(x = name,
               value = get(object,
                           envir = tmpenv,
                           inherits = FALSE),
               envir = envir)
        names(file) <- name
        file
    })
    invisible(loaded)
}
