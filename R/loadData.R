#' Load Data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports "`.RData`", "`.rda`", and "`.rds`" file extensions.
#'
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [base::save()] allows for the saving of multiple objects into a single R data
#' file. This can later result in unexpected accidental replacement of an
#' existing object in the current environment. Additionally, since an R data
#' file internally stores the name of an object, if the file is later renamed
#' the object name will no longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param ... Object names. Note that these arguments are interpreted as symbols
#'   using non-standard evaluation for convenience during interactive use, and
#'   *should not be quoted*.
#'
#' @return Invisible `character` containing file paths.
#' @export
#'
#' @examples
#' loadData(
#'     anorexia, birthwt,
#'     dir = system.file("tests", package = "stats")
#' )
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    assert_is_environment(envir)

    dots <- dots(..., character = TRUE)

    # Match rda, rdata, rds extensions
    extPattern <- "\\.(rd[a|ata|s])$"
    files <- list.files(
        path = dir,
        pattern = paste0(
            "^(",
            paste(dots, collapse = "|"),
            ")",
            extPattern
        ),
        full.names = TRUE,
        ignore.case = TRUE
    )
    names <- gsub(extPattern, "", basename(files))
    names(files) <- names

    # Check for duplicate names
    if (any(duplicated(names))) {
        dupeNames <- names[duplicated(names)]
        dupeFiles <- grep(
            paste(dupeNames, collapse = "|"),
            basename(files),
            value = TRUE
        )
        stop(paste(
            "Duplicates",
            toString(dupeFiles),
            sep = " : "
        ))
    }

    # Check for extension soup and stop on detection
    ext <- str_match(files, extPattern) %>%
        .[, 2L] %>%
        unique() %>%
        sort()
    if (length(ext) != 1L) {
        stop(paste(
            paste(
                "Multiple extensions",
                toString(ext),
                sep = " : "
            ),
            "Use a single R data file format inside a directory.",
            printString(files),
            sep = "\n"
        ))
    }

    # Now safe to sort the files to match the dots
    files <- files[dots]
    message(paste("Loading", toString(basename(files)), "from", dir))

    # R Data
    if (ext == "rds") {
        mapply(
            FUN = .safeLoadRDS,
            file = files,
            MoreArgs = list(envir = envir),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
    } else {
        mapply(
            FUN = .safeLoad,
            file = files,
            MoreArgs = list(envir = envir),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
    }

    invisible(files)
}
