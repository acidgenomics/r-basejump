.listRData <- function(
    dots,
    dir = "."
) {
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

    # Match rda, rdata, rds extensions
    files <- list.files(
        path = dir,
        pattern = paste0(
            "^(",
            paste(dots, collapse = "|"),
            ")",
            rdataExtPattern
        ),
        full.names = TRUE,
        ignore.case = TRUE
    )
    if (!has_length(files)) {
        stop(rdataError, call. = FALSE)
    }
    names <- gsub(rdataExtPattern, "", basename(files), ignore.case = TRUE)
    names(files) <- names

    # Check for duplicate names
    if (any(duplicated(names))) {
        dupeNames <- names[duplicated(names)]
        dupeFiles <- grep(
            paste(dupeNames, collapse = "|"),
            basename(files),
            value = TRUE
        )
        stop(paste0("Duplicates: ", toString(dupeFiles)))
    }

    # Check for extension soup and stop on detection
    ext <- files %>%
        str_match(regex(rdataExtPattern, ignore_case = TRUE)) %>%
        .[, 2L] %>%
        unique() %>%
        sort()
    if (!has_length(ext, n = 1L)) {
        stop(paste(
            paste0("Multiple extensions: ", toString(ext)),
            "Use a single R data file format inside a directory.",
            printString(files),
            sep = "\n"
        ))
    }

    # Now safe to sort the files to match the dots
    files <- files[dots]
    message(paste0("Loading ", toString(basename(files)), " from ", dir, "."))
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

    # Fail on attempt to load on top of an existing object
    assertAllAreNonExisting(name, envir = envir, inherits = FALSE)

    # Load into a temporary environment
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)

    # Ensure that the loaded name is identical to the file name
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

    # Now we're ready to assign into the target environment
    assign(
        x = name,
        value = get(name, envir = tmpEnvir, inherits = FALSE),
        envir = envir
    )

    # Ensure that assign worked
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

    # Fail on attempt to load on top of an existing object
    assertAllAreNonExisting(name, envir = envir, inherits = FALSE)

    assign(
        x = name,
        value = data,
        envir = envir
    )

    # Ensure that assign worked
    assert_all_are_existing(
        x = name,
        envir = envir,
        inherits = FALSE
    )

    file
}
