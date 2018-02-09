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
#' loadDataAsName(
#'     annotable = "grch37",
#'     dir = system.file("extdata", package = "basejump")
#' )
#' glimpse(annotable)
loadDataAsName <- function(
    ...,
    dir = getwd(),
    ext = "rda",
    envir = parent.frame(),
    replace = TRUE) {
    dots <- list(...)
    dir <- .sanitizeDir(dir)
    .checkExt(ext)
    .checkEnvir(envir)
    .checkReplace(replace)
    # Check for legacy mappings method, used prior to v0.1.1
    if (length(dots) == 1L & !is.null(names(dots[[1L]]))) {
        # Convert the named character vector to a named list, for consistency
        dots <- as.list(dots[[1L]])
    }
    files <- sapply(seq_along(dots), function(a) {
        object <- dots[[a]]
        name <- names(dots)[[a]]
        # Check to see if full file path was passed
        fileExtPattern <- paste0("\\.", ext, "$")
        if (grepl(fileExtPattern, object)) {
            file <- object
            # Extract the object name from the file name
            object <- gsub(fileExtPattern, "", basename(object))
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
        assign(
            x = name,
            value = get(loaded, envir = tmpEnv, inherits = FALSE),
            envir = envir
        )
        # Prepare named character vector for invisible return
        names(file) <- name
        file
    })
    invisible(files)
}
