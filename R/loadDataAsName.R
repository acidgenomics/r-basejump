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
#' # Simply specify the key-value pairs as arguments, and set the directory path
#' # (`dir`) where the R data files saved. This example will load `mtcars.rda`
#' # and `starwars.rda` files saved at `data/` as `newName1` and `newName2` in
#' # the working environment.
#' \dontrun{
#' loadDataAsName(
#'     newName1 = "mtcars",
#'     newName2 = "starwars",
#'     dir = "data"
#' )
#' }
#'
#' # Requesting the file path directly also works. In this case, the `dir`
#' # argument wil be ignored.
#' \dontrun{
#' loadDataAsName(newName = "data/mtcars.rda")
#' }
#'
#' # Legacy mappings method using a named character vector is still supported.
#' \dontrun{
#' loadDataAsName(
#'     c(newName1 = "mtcars",
#'       newName2 = "starwars"),
#'     dir = "data"
#' )
#' }
loadDataAsName <- function(
    ...,
    dir = getwd(),
    ext = "rda",
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
    tmpEnv <- new.env()
    loaded <- sapply(seq_along(mappings), function(a) {
        object <- mappings[[a]]
        name <- names(mappings)[[a]]
        # Check to see if full file path was passed
        fileExtPattern <- paste0("\\.", ext, "$")
        if (grepl(x = object, pattern = fileExtPattern)) {
            file <- object
            # Extract the object name from the file name
            object <- gsub(
                x = basename(object),
                pattern = fileExtPattern,
                replacement = "")
        } else {
            file <- file.path(dir, paste0(object, paste0(".", ext)))
        }
        if (!file.exists(file)) {
            stop(paste(object, "missing"), call. = FALSE)
        }
        file <- normalizePath(file)
        tmpName <- load(file, envir = tmpEnv)
        assign(x = name,
               value = get(tmpName, envir = tmpEnv, inherits = FALSE),
               envir = envir)
        names(file) <- name
        file
    })
    invisible(loaded)
}
