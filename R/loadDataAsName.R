#' Load Data File as Name
#'
#' @importFrom rlang eval_bare
#'
#' @inheritParams loadData
#'
#' @param ... Key value pairs, defining the name mappings. The argument name
#'   defines the new name of the object in the environment, whereas the value
#'   (string) denotes the original object name. For example, `newName1 =
#'   "oldName1", newName2 = "oldName2"`.
#' @param envir The environment where the data should be loaded.
#'
#' @return Silently return named character vector of file paths.
#' @export
#'
#' @examples
#' # New dots method (>= 0.1.1)
#' \dontrun{
#' loadDataAsName(foo = "mtcars", bar = "starwars")
#' }
#'
#' # Legacy named character vector mappings method
#' \dontrun{
#' loadDataAsName(c(foo = "mtcars", bar = "starwars"))
#' }
loadDataAsName <- function(
    ...,
    dir = "data",
    envir = parent.frame()) {
    mappings <- list(...)
    # Check for legacy mappings method, used prior to v0.1.1
    if (length(mappings) == 1 & !is.null(names(mappings[[1]]))) {
        # Convert the named character vector to a named list, for consistency
        mappings <- as.list(mappings[[1]])
    }
    # Assign into a temporary environment, rather than using `attach()`
    if (!is.environment(envir)) {
        stop("'envir' must be an environment", call. = FALSE)
    }
    tmpenv <- new.env()
    loaded <- sapply(seq_along(mappings), function(a) {
        object <- mappings[[a]]
        name <- names(mappings)[[a]]
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
               value = get(object, envir = tmpenv, inherits = FALSE),
               envir = envir)
        names(file) <- name
        file
    })
    invisible(loaded)
}
