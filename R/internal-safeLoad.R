.safeLoad <- function(
    file,
    envir = parent.frame(),
    replace = FALSE) {
    assert_is_a_string(file)
    assert_all_are_existing_files(file)
    assert_is_environment(envir)
    assert_is_a_bool(replace)

    name <- gsub("\\.rda$", "", basename(file))

    # Check to see if object is present in environment
    if (exists(name, envir = envir, inherits = FALSE)) {
        if (isTRUE(replace)) {
            warn(paste(
                "Replacing", name, "in", deparse(substitute(envir)),
                "with the contents of", basename(file)
            ))
        } else {
            warn(paste(
                "Skipping", basename(file),
                "because", name, "already exists in",
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
