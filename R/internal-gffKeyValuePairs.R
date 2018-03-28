.gffKeyValuePairs <- function(gff, unique = FALSE) {
    assertIsGFF(gff)
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
    list <- mclapply(list, function(string) {
        match <- str_match(string, "(.+)\\s(.+)")
        keys <- match[, 2L]
        # Enforce camel case
        keys <- camel(keys)
        values <- match[, 3L]
        names(values) <- keys
        values
    })

    list %>%
        ldply(rbind) %>%
        as.data.frame(stringsAsFactors = TRUE)
}
