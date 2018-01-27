#' Load Data File as Name
#'
#' @inheritParams loadData
#'
#' @param ... Key value pairs, defining the name mappings. The argument name
#'   defines the new name of the object in the environment, whereas the value
#'   (string) denotes the original object name. For example, `newName1 =
#'   "oldName1", newName2 = "oldName2"`.
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
    envir = parent.frame(),
    replace = TRUE) {
    dots <- list(...)
    # Check for legacy mappings method, used prior to v0.1.1
    if (length(dots) == 1L & !is.null(names(dots[[1L]]))) {
        # Convert the named character vector to a named list, for consistency
        dots <- as.list(dots[[1L]])
    }
    if (!is_string(dir)) {
        abort("`dir` must be a string")
    } else if (!dir.exists(dir)) {
        abort(paste("No directory exists at", dir))
    } else {
        dir <- normalizePath(dir)
    }
    if (!is.environment(envir)) {
        abort("`envir` must be an environment")
    }
    files <- sapply(seq_along(dots), function(a) {
        object <- dots[[a]]
        name <- names(dots)[[a]]
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
            abort(paste(object, "missing"))
        }
        file <- normalizePath(file)
        # Check to see if object is present in environment
        if (exists(name, envir = envir, inherits = FALSE)) {
            if (isTRUE(replace)) {
                warn(paste(
                    "Replacing", name,
                    "with the contents of", basename(file)
                ))
            } else {
                return(warn(paste(
                    "Skipping", basename(file),
                    "because", name, "already exists"
                )))
            }
        }
        # Load into a temporary environment (safer)
        tmpEnv <- new.env()
        loaded <- load(file, envir = tmpEnv)
        # Check for multiple saved objects
        if (length(loaded) > 1L) {
            abort(paste(
                basename(file), "contains multiple objects:",
                toString(loaded)
            ))
        }
        # Assign into the target environment
        assign(x = name,
               value = get(loaded, envir = tmpEnv, inherits = FALSE),
               envir = envir)
        # Prepare named character vector for invisible return
        names(file) <- name
        file
    })
    invisible(files)
}
