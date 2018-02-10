.safeLoad <- function(
    file,
    name = NULL,
    envir = parent.frame(),
    replace = FALSE) {
    assert_is_a_string(file)
    assert_is_any_of(name, c("character", "NULL"))
    assert_all_are_existing_files(file)
    assert_is_environment(envir)
    assert_is_a_bool(replace)

    # Get the name from the file stem
    if (is.null(name)) {
        extPattern <- "\\.rda$"
        assert_all_are_matching_regex(name, extPattern)
        name <- gsub(extPattern, "", basename(file))
    }

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

    # Load into a temporary environment
    tmpEnvir <- new.env()
    loaded <- load(file, envir = tmpEnvir)
    assert_is_a_string(loaded)
    assert_are_identical(name, loaded)

    # Now we're ready to assign into the target environment
    assign(
        x = name,
        value = get(name, envir = tmpEnvir, inherits = FALSE),
        envir = envir
    )

    file
}
