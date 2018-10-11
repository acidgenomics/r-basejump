#' Load Data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports "`.rds`", "`.rda`", and "`.RData`" file extensions.
#'
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [save()] allows for the saving of multiple objects into a single R data file.
#' This can later result in unexpected accidental replacement of an existing
#' object in the current environment. Since an R data file internally stores the
#' name of an object, if the file is later renamed the object name will no
#' longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. Additionally, [loadData()] will intentionally error if
#' an object with the same name already exists in the destination `environment`.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @family Import/Export Functions
#' @export
#'
#' @inheritParams general
#' @param ... Object names. Note that these arguments are interpreted as symbols
#'   using non-standard evaluation for convenience during interactive use, and
#'   *must not be quoted*.
#'
#' @return Invisible `character`. File paths.
#'
#' @seealso [load], [readRDS].
#'
#' @examples
#' loadData(example, dir = system.file("extdata", package = "basejump"))
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    assert_is_environment(envir)
    names <- dots(..., character = TRUE)
    files <- .listRData(names = names, dir = dir)
    lapply(
        X = files,
        FUN = function(file) {
            if (grepl("\\.rds$", file)) {
                fun <- .safeLoadRDS
            } else {
                fun <- .safeLoad
            }
            fun(file, envir = envir)
        }
    )
    invisible(files)
}



#' Load Data as Name
#'
#' @note This function is intended for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @family Import/Export Functions
#' @export
#'
#' @inheritParams loadData
#' @param ... Key value pairs, defining the name mappings. For example,
#'   `newName1` = `oldName1`, `newName2` = `oldName2`. Note that these
#'   arguments are interpreted using non-standard evaluation, and *should not
#'   be quoted*.
#'
#' @return Invisible named `character`. File paths.
#'
#' @examples
#' loadDataAsName(
#'     renamed = example,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' class(renamed)
loadDataAsName <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    dots <- dots(..., character = TRUE)
    assert_has_names(dots)
    files <- .listRData(names = dots, dir = dir)
    names(files) <- names(dots)
    assert_is_environment(envir)

    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(names(dots), envir = envir, inherits = FALSE)

    if (any(grepl("\\.rds$", files))) {
        # R data serialized: assign directly
        invisible(mapply(
            name = names(files),
            file = files,
            FUN = function(name, file, envir) {
                data <- readRDS(file)
                assign(
                    x = name,
                    value = data,
                    envir = envir
                )
            },
            MoreArgs = list(envir = envir)
        ))
    } else {
        # R data: use safe loading
        safe <- new.env()
        invisible(mapply(
            FUN = .safeLoad,
            file = files,
            MoreArgs = list(envir = safe)
        ))
        assert_are_set_equal(dots, ls(safe))

        # Now assign to the desired object names
        invisible(mapply(
            FUN = function(from, to, safe, envir) {
                assign(
                    x = to,
                    value = get(from, envir = safe, inherits = FALSE),
                    envir = envir
                )
            },
            from = dots,
            to = names(dots),
            MoreArgs = list(safe = safe, envir = envir),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        ))
    }

    invisible(files)
}



#' Load Remote Data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @family Import/Export Functions
#' @export
#'
#' @inheritParams general
#' @param url `character`. Remote URL file path(s) to R data.
#'
#' @return Invisible named `character`. Local object name as the name, and the
#'   remote URL as the value.
#'
#' @examples
#' url <- file.path(
#'     basejumpCacheURL,
#'     paste0(c("rnaseq_counts", "single_cell_counts"), ".rds")
#' )
#' print(url)
#' x <- loadRemoteData(url)
#' print(x)
loadRemoteData <- function(url, envir = parent.frame()) {
    stopifnot(has_internet())
    assertAllAreURL(url)
    if (!all(vapply(
        X = url,
        FUN = function(x) {
            grepl(rdataExtPattern, x, ignore.case = TRUE)
        },
        FUN.VALUE = logical(1L)
    ))) {
        stop(rdataError, call. = FALSE)
    }
    assert_is_environment(envir)
    names <- gsub(rdataExtPattern, "", basename(url), ignore.case = TRUE)
    names(url) <- names

    # Check to make sure the objects don't already exist.
    assertAllAreNonExisting(names, envir = envir, inherits = FALSE)

    # Download the files to tempdir and return a character matrix of mappings.
    invisible(mapply(
        name = names,
        url = url,
        MoreArgs = list(envir = envir),
        FUN = function(name, url, envir) {
            data <- import(url)
            assign(x = name, value = data, envir = envir)
        }
    ))

    assert_all_are_existing(names, envir = envir, inherits = FALSE)
    invisible(url)
}



# Internal =====================================================================
.listRData <- function(
    names,
    dir = "."
) {
    assert_is_character(names)
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    files <- vapply(
        X = names,
        FUN = function(name) {
            files <- list.files(
                path = dir,
                pattern = paste0("^", name, rdataExtPattern),
                full.names = TRUE,
                ignore.case = TRUE
            )
            # Add error checking here.
            if (!has_length(files)) {
                stop(paste0(
                    name, " is missing.\n",
                    rdataError
                ), call. = FALSE)
            } else if (length(files) > 1L) {
                stop(paste0(
                    name, " is not unique on disk.\n",
                    rdataError
                ), call. = FALSE)
            }
            files
        },
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
    )
    message(paste0(
        "Loading ",
        toString(basename(files)),
        " from ",
        dir, "..."
    ))
    files
}



.safeLoad <- function(
    file,
    name = NULL,
    envir = parent.frame()
) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    assertIsAStringOrNULL(name)
    assert_is_environment(envir)

    if (is.null(name)) {
        stopifnot(grepl(rdataExtPattern, file, ignore.case = TRUE))
        name <- gsub(rdataExtPattern, "", basename(file))
    }

    # Fail on attempt to load on top of an existing object.
    assertAllAreNonExisting(name, envir = envir, inherits = FALSE)

    # Load into a temporary environment.
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)

    # Ensure that the loaded name is identical to the file name.
    if (!is_a_string(loaded)) {
        stop(paste0(
            basename(file),
            " contains multiple objects: ",
            toString(loaded)
        ))
    }
    if (!identical(name, loaded)) {
        stop(paste0(
            basename(file), " has been renamed.\n",
            "The object name inside the file doesn't match.\n",
            "  expected: ", name, "\n",
            "    actual: ", loaded, "\n",
            "Avoid renaming R data files. ",
            "This can lead to accidental replacement."
        ))
    }
    assert_are_identical(name, loaded)

    # Now we're ready to assign into the target environment.
    assign(
        x = name,
        value = get(name, envir = tmpEnvir, inherits = FALSE),
        envir = envir
    )

    # Ensure that assign worked.
    assert_all_are_existing(
        x = name,
        envir = envir,
        inherits = FALSE
    )

    file
}



.safeLoadRDS <- function(file, envir = parent.frame()) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    assert_is_environment(envir)

    name <- gsub("\\.rds", "", basename(file), ignore.case = TRUE)
    data <- readRDS(file)

    # Fail on attempt to load on top of an existing object.
    assertAllAreNonExisting(name, envir = envir, inherits = FALSE)

    assign(x = name, value = data, envir = envir)

    # Ensure that assign worked.
    assert_all_are_existing(x = name, envir = envir, inherits = FALSE)

    file
}
