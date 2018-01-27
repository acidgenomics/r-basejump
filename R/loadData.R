#' Load Local Data
#'
#' Load RData (`.rda`) files from a directory using symbols rather than complete
#' file paths.
#'
#' @details
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [base::save()] allows for the saving of multiple objects into a single R data
#' file. This can later result in unexpected accidental replacement of an
#' existing object in the current environment. Additionally, since an R data
#' file internally stores the name of an object, if the file is later renamed
#' the object name will no longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. These conventions match the recommendations of the
#' RStudio team, which recommends saving single objects per file.
#'
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to the current working directory.
#' @param ext R data file extension. Defaults to `rda` and typically should not
#'   be changed.
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#'   which will assign into the calling environment.
#' @param replace Replace existing object in destination environment.
#' @param quiet If `TRUE`, suppress any status messages and/or progress bars.
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' # Load the internal synonyms data
#' loadData(synonyms, dir = system.file("data", package = "basejump"))
loadData <- function(
    ...,
    dir = getwd(),
    ext = "rda",
    envir = parent.frame(),
    replace = TRUE,
    quiet = FALSE) {
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
    # The dots method will error at this step because the objects (as symbols)
    # aren't present in the calling environment
    dots <- as.character(substitute(list(...)))[-1L]
    if (!isTRUE(quiet)) {
        inform(paste("Loading", toString(dots), "from", dir))
    }
    files <- vapply(
        X = seq_along(dots),
        FUN = function(a) {
            name <- dots[[a]]
            file <- file.path(dir, paste0(name, ".", ext))
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
            # Error on missing file
            if (!file.exists(file)) {
                abort(paste(name, "missing"))
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
            # Check for file name and internal object name mismatch
            if (!identical(name, loaded)) {
                abort(paste0(
                    "Name mismatch detected for `", basename(file), "`. ",
                    "Internal object is named `", loaded, "`."
                ))
            }
            # Assign into the target environment
            assign(x = name,
                   value = get(name, envir = tmpEnv, inherits = FALSE),
                   envir = envir)
            # Prepare named character vector for invisible return
            names(file) <- name
            file
        },
        FUN.VALUE = "character")
    invisible(files)
}
