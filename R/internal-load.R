.listRData <- function(
    dots,
    dir = "."
) {
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

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
}



.safeLoad <- function(
    file,
    name = NULL,
    envir = parent.frame()
) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    assert_is_any_of(name, c("character", "NULL"))
    if (is.character(name)) {
        assert_is_a_string(name)
    }
    assert_is_environment(envir)

    # Get the name from the file stem. Supports `.rdata` and `.rda`
    extPattern <- "\\.rd[a|ata]$"
    if (is.null(name)) {
        stopifnot(grepl(extPattern, file, ignore.case = TRUE))
        name <- gsub(extPattern, "", basename(file))
    }

    # Fail on attempt to load on top of an existing object
    assertAllAreNonExisting(name, envir = envir, inherits = FALSE)

    # Load into a temporary environment
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)

    # Ensure that the loaded name is identical to the file name
    assert_is_a_string(loaded)
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
