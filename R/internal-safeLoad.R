.safeLoad <- function(
    file,
    name = NULL,
    envir = parent.frame()
) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    assert_is_any_of(name, c("character", "NULL"))
    if (is.character(name)) {
        assert_is_a_string(name)
    }
    assert_is_environment(envir)

    # Get the name from the file stem
    if (is.null(name)) {
        assert_all_are_matching_regex(file, "\\.rda$")
        name <- gsub("\\.rda$", "", basename(file))
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

    file
}
