#' Load Data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports "`.rds`", "`.rda`", and "`.RData`" file extensions.
#'
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [base::save()] allows for the saving of multiple objects into a single R data
#' file. This can later result in unexpected accidental replacement of an
#' existing object in the current environment. Since an R data file internally
#' stores the name of an object, if the file is later renamed the object name
#' will no longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. Additionally, [loadData()] will intentionally error if
#' an object with the same name already exists in the destination `environment`.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @export
#'
#' @inheritParams params
#' @param ... Object names. Note that these arguments are interpreted as symbols
#'   using non-standard evaluation for convenience during interactive use, and
#'   *must not be quoted*.
#'
#' @return Invisible `character`. File paths.
#'
#' @seealso [base::load()], [base::readRDS()].
#'
#' @examples
#' loadData(example, dir = system.file("extdata", package = "basejump.io"))
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



.safeLoad <- function(
    file,
    name = NULL,
    envir = parent.frame()
) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    file <- realpath(file)
    assertIsAStringOrNULL(name)
    assert_is_environment(envir)

    if (is.null(name)) {
        assert_that(grepl(rdataExtPattern, file, ignore.case = TRUE))
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
    file <- realpath(file)
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
