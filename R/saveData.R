#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams loadData
#' @inheritParams base::save
#' @param overwrite `boolean`. Overwrite existing file.
#' @param ext `string`. R data serialized (RDS; "`rds`") or R data ("`rda`",
#'   "`RData`"). RDS is preferred when saving single objects per file, which is
#'   always the convention of [saveData()], regardless of the extension used.
#'
#' @note
#' This function is desired for interactive use and interprets object names
#' using non-standard evaluation. It will *overwrite* existing saved data,
#' following the same conventions as [base::save()]. Conversely,
#' [devtools::use_data()] does not overwrite by default if that behavior is
#' preferred.
#'
#' @seealso
#' - [base::save()].
#' - `usethis::use_data()`.
#'
#' @return Invisible named `character`. File paths.
#'
#' @examples
#' saveData(rse_small, sce_small, dir = "example")
#' list.files("example")
#'
#' # Clean up
#' unlink("example", recursive = TRUE)
saveData <- function(
    ...,
    dir = getOption("basejump.save.dir", "."),
    ext = getOption("basejump.save.ext", "rds"),
    overwrite = getOption("basejump.save.overwrite", TRUE),
    compress = getOption("basejump.save.compress", TRUE)
) {
    objects <- list(...)
    names(objects) <- dots(..., character = TRUE)
    dir <- initializeDirectory(dir)
    ext <- match.arg(arg = ext, choices = c("rds", "rda", "RData"))
    assert_is_a_bool(overwrite)
    assertFormalCompress(compress)

    files <- file.path(dir, paste(names(objects), ext, sep = "."))
    names(files) <- names(objects)

    message(paste0("Saving ", toString(basename(files)), " to ", dir, "..."))

    # If `overwrite = FALSE`, inform the user which files were skipped
    if (identical(overwrite, FALSE) && any(file.exists(files))) {
        skip <- files[file.exists(files)]
        warning(paste0(
            "Skipped ", toString(basename(skip)), "."
        ), call. = FALSE)
        files <- files[!file.exists(files)]
        if (!has_length(files)) {
            warning("No files were saved.")
            return(invisible())
        }
        objects <- objects[!file.exists(files)]  # nocov
    }

    # Determine which save function to use.
    if (ext == "rds") {
        mapply(
            FUN = saveRDS,
            object = objects,
            file = files,
            MoreArgs = list(
                compress = compress
            )
        )
    } else {
        mapply(
            FUN = save,
            list = names(files),
            file = files,
            MoreArgs = list(
                envir = parent.frame(),
                compress = compress
            )
        )
    }

    invisible(files)
}



#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the "`dir`" argument.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams saveData
#' @inheritParams general
#' @param name `string`. Desired variable name.
#' @param envir `environment`. Environment to use for assignment. Defaults to
#'   [parent.frame()], the calling environment.
#'
#' @return Invisible named `string`. File path.
#'
#' @note This function attempts to follow the same order as [base::assign()].
#'
#' @examples
#' assignAndSaveData(name = "example", object = rse_small, ext = "rds")
#' exists("example", inherits = FALSE)
#' file.exists("example.rds")
#'
#' # Clean up
#' rm(example)
#' unlink("example.rds")
assignAndSaveData <- function(
    name,
    object,
    envir = parent.frame()
) {
    assert_is_a_string(name)
    assert_is_not_null(object)
    dir <- initializeDirectory(dir)
    assertFormalCompress(compress)
    assert_is_environment(envir)

    # Assign data.
    assign(x = name, value = object, envir = envir)
    assign(x = name, value = object)

    # Save data.
    args <- list(
        as.name(name),
        dir = dir,
        ext = ext,
        overwrite = overwrite,
        compress = compress
    )
    file <- do.call(what = saveData, args = args)

    invisible(file)
}

# Assign the formals.
f1 <- formals(assignAndSaveData)
f2 <- formals(saveData)
f2 <- f2[setdiff(names(f2), "...")]
f <- c(f1, f2)
formals(assignAndSaveData) <- f
