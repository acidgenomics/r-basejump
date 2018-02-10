#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams loadData
#'
#' @param url Remote URL file path to R Data file. Supports multiple URLs
#'   passed in as a character vector.
#'
#' @return Silently return a character matrix containing URL and tempfile paths.
#' @export
#'
#' @examples
#' objects <- loadRemoteData(c(
#'     "http://basejump.seq.cloud/mtcars.rda",
#'     "http://basejump.seq.cloud/starwars.rda"
#' ))
#' objects
loadRemoteData <- function(
    url,
    envir = parent.frame(),
    replace = FALSE,
    quiet = FALSE) {
    assert_is_character(url)
    # Check for remote URL containing `.rda` file
    assert_all_are_matching_regex(url, "^http(s)?\\://.+\\.rda$")
    assert_is_environment(envir)
    assert_is_a_bool(quiet)

    extPattern <- "\\.rda$"

    .downloadRData <- function(
        url,
        envir = parent.frame(),
        replace = FALSE,
        quiet = FALSE) {
        assert_is_a_string(url)
        assert_is_a_bool(quiet)
        name <- gsub(extPattern, "", basename(url))
        # Check to see if object is present in environment
        if (exists(name, envir = envir, inherits = FALSE)) {
            if (isTRUE(replace)) {
                warn(paste(
                    "Replacing", name, "in", deparse(substitute(envir))
                ))
            } else {
                warn(paste(
                    "Skipping", name, "because it already exists in",
                    deparse(substitute(envir))
                ))
                return(NULL)
            }
        }
        tempfile <- tempfile()
        download.file(
            url = url,
            destfile = tempfile,
            quiet = quiet)
        c(url = url, tempfile = tempfile)
    }

    # Download the files to tempdir
    files <- mapply(
        FUN = .downloadRData,
        url = url,
        MoreArgs = list(envir = envir, replace = replace, quiet = quiet),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    files <- Filter(Negate(is.null), files)
    assert_is_list(files)
    if (!length(files)) {
        return(invisible())
    }
    files <- do.call(rbind, files)
    assert_is_matrix(files)
    rownames(files) <- gsub(extPattern, "", basename(files[, "url"]))

    names <- rownames(files)
    tempfiles <- files[, "tempfile"]

    # Now we're ready to load safely from the tempdir
    objects <- mapply(
        FUN = .safeLoad,
        file = tempfiles,
        name = names,
        MoreArgs = list(envir = envir, replace = replace),
        USE.NAMES = TRUE
    )

    return <- files[names(objects), , drop = FALSE]
    invisible(return)
}
