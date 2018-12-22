#' Load data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports "`.rds`", "`.rda`", and "`.RData`" file extensions.
#'
#' `loadData` is opinionated about the format of R data files it will accept.
#' `save` allows for the saving of multiple objects into a single R data file.
#' This can later result in unexpected accidental replacement of an existing
#' object in the current environment. Since an R data file internally stores the
#' name of an object, if the file is later renamed the object name will no
#' longer match.
#'
#' To avoid any accidental replacements, `loadData` will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. Additionally, `loadData` will intentionally error if
#' an object with the same name already exists in the destination `environment`.
#'
#' @note This function is desired for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @inheritParams params
#' @export

#' @param ... Object names.
#'   Note that these arguments are interpreted as symbols using non-standard
#'   evaluation for convenience during interactive use, and *must not be
#'   quoted*.
#'
#' @return Invisible `character`.
#' File paths.
#'
#' @seealso `load`, `readRDS`.
#'
#' @examples
#' loadData(example, dir = system.file("extdata", package = "basejump"))
loadData <- function(..., dir, envir = globalenv()) {
    names <- dots(..., character = TRUE)
    files <- .listData(names = names, dir = dir)
    assert(is.environment(envir))
    if (all(grepl(
        pattern = "\\.rds$",
        x = files,
        ignore.case = TRUE
    ))) {
        fun <- .safeLoadRDS
    } else if (all(grepl(
        pattern = "\\.rd[a|ata]$",
        x = files,
        ignore.case = TRUE
    ))) {
        fun <- .safeLoadRDA
    } else {
        stop(paste0(
            "File extension error: ",
            toString(basename(files)), "\n",
            "Don't mix RDS/RDA/RDATA files in a single directory."
        ))
    }
    lapply(X = files, FUN = fun, envir = envir)
    assert(allAreExisting(names, envir = envir, inherits = FALSE))
    invisible(files)
}

formals(loadData)[["dir"]] <- formalsList[["load.dir"]]



#' Load data as name
#'
#' @note This function is intended for interactive use and interprets object
#'   names using non-standard evaluation.
#'
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
loadDataAsName <- function(..., dir, envir = globalenv()) {
    dots <- dots(..., character = TRUE)
    assert(hasNames(dots))
    files <- .listData(names = dots, dir = dir)
    names(files) <- names(dots)
    assert(is.environment(envir))

    # Check to see if any of the new names already exist in environment.
    names <- names(dots)
    if (!isTRUE(allAreNonExisting(names, envir = envir, inherits = FALSE))) {
        .safeLoadExistsError(names)
    }

    # Note that we can skip safe loading here because we have already checked
    # for existing names in environment outside of the loop call.
    if (any(grepl("\\.rds$", files))) {
        # R data serialized: assign directly.
        invisible(mapply(
            name = names(files),
            file = files,
            FUN = function(name, file, envir) {
                data <- readRDS(file)
                assign(x = name, value = data, envir = envir)
            },
            MoreArgs = list(envir = envir)
        ))
    } else {
        # R data: use safe loading.
        safe <- new.env()
        invisible(mapply(
            FUN = .safeLoadRDA,
            file = files,
            MoreArgs = list(envir = safe)
        ))
        assert(areSetEqual(dots, ls(safe)))

        # Now assign to the desired object names.
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

formals(loadDataAsName)[["dir"]] <- formalsList[["load.dir"]]



#' Load remote data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @export
#'
#' @inheritParams params
#' @param url `character`. Remote URL file path(s) to R data.
#'
#' @return Invisible named `character`. Local object name as the name, and the
#'   remote URL as the value.
#'
#' @examples
#' url <- pasteURL(basejumpCacheURL, "rnaseq_counts.rds", protocol = "none")
#' print(url)
#' x <- loadRemoteData(url)
#' print(x)
loadRemoteData <- function(url, envir = globalenv()) {
    assert(
        hasInternet(),
        allAreURLs(url),
        is.environment(envir)
    )
    if (!all(vapply(
        X = url,
        FUN = function(x) {
            grepl(rdataExtPattern, x, ignore.case = TRUE)
        },
        FUN.VALUE = logical(1L)
    ))) {
        stop(rdataLoadError, call. = FALSE)
    }
    names <- gsub(rdataExtPattern, "", basename(url), ignore.case = TRUE)
    names(url) <- names

    # Check to make sure the objects don't already exist.
    if (!isTRUE(allAreNonExisting(names, envir = envir, inherits = FALSE))) {
        .safeLoadExistsError(names)
    }

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

    assert(allAreExisting(names, envir = envir, inherits = FALSE))
    invisible(url)
}



.listData <- function(names, dir) {
    assert(isCharacter(names))
    dir <- realpath(dir)
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
            if (length(files) == 0L) {
                stop(paste0(
                    name, " is missing.\n",
                    rdataLoadError
                ), call. = FALSE)
            } else if (length(files) > 1L) {
                stop(paste0(
                    name, " is not unique on disk.\n",
                    rdataLoadError
                ), call. = FALSE)
            }
            files
        },
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
    )
    message(paste("Loading", toString(basename(files)), "from", dir))
    files
}



.safeLoadExistsError <- function(name) {
    stop(paste0(
        deparse(name), " exists in environment.\n",
        "The basejump load functions do not allow reassignment.\n",
        "We recommending either adding a rm() step or using readRDS()."
    ), call. = FALSE)
}



.safeLoadRDS <- function(file, envir) {
    file <- realpath(file)
    assert(
        isAFile(file),
        # Allowing RDS only here.
        grepl("\\.rds$", file, ignore.case = TRUE),
        is.environment(envir)
    )
    name <- basenameSansExt(file)
    data <- readRDS(file)
    # Always error if the object is already assigned in environment.
    if (exists(x = name, envir = envir, inherits = FALSE)) {
        .safeLoadExistsError(name)
    }
    assign(x = name, value = data, envir = envir)
    assert(exists(x = name, envir = envir, inherits = FALSE))
    invisible(file)
}



.safeLoadRDA <- function(file, name = NULL, envir) {
    file <- realpath(file)
    assert(
        isAFile(file),
        # Allowing RDA or RDATA here.
        grepl("\\.rd[a|ata]$", file, ignore.case = TRUE),
        isString(name, nullOK = TRUE),
        is.environment(envir)
    )
    if (is.null(name)) {
        name <- basenameSansExt(file)
    }
    # Always error if the object is already assigned in environment.
    if (exists(x = name, envir = envir, inherits = FALSE)) {
        .safeLoadExistsError(name)
    }

    # Loading into a temporary environment, so we can evaluate the integrity
    # of the objects before assigning into the destination environment.
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)

    # Ensure that the loaded name is identical to the file name.
    if (!isString(loaded)) {
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
    assert(identical(name, loaded))

    # Now we're ready to assign into the target environment.
    assign(
        x = name,
        value = get(name, envir = tmpEnvir, inherits = FALSE),
        envir = envir
    )

    # Ensure that assignment worked.
    assert(exists(x = name, envir = envir, inherits = FALSE))

    file
}
