# Deparsing unique values is much faster and generally recommended
.gffKeyValuePairs <- function(gff, unique = TRUE) {
    assertIsGFF(gff)

    inform("Parsing GFF key value pairs")
    strings <- gff %>%
        .[["keyValuePairs"]] %>%
        as.character()

    # This can be significantly faster for large GFF files
    if (isTRUE(unique)) {
        strings <- unique(strings)
    }

    list <- strings %>%
        # Remove trailing semicolon
        gsub(";$", "", .) %>%
        strsplit("; ")

    # Now ready to iterate across the list and generate key value pairs as
    # a named character vector. This can take a long time for large GFF files.
    list <- pblapply(list, function(string) {
        # Key value pairs are separated by a space
        match <- str_match(string, "^([^\\s]+)\\s([^\\s]+)$")
        keys <- match[, 2L]
        values <- match[, 3L]
        names(values) <- keys
        values
    })

    ldply(list, rbind) %>%
        # Enforce camel case
        camel() %>%
        # Return strings instead of factors
        mutate_if(is.factor, as.character)
}
