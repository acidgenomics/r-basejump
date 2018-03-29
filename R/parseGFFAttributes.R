#' Parse GFF Attributes Column
#'
#' @note Memory overhead and speed is greatly improved by requiring keys.
#' Deparsing only unique values is much faster and generally recommended.
#'
#' @param select Attribute keys to select. Supports partial matching. Defaults
#'   to returning attributes matching both "`gene_`" and "`transcript_`".
#' @param unique Return unique attributes.
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' parseGFFAttributes("mmusculus.gtf") %>% glimpse()
parseGFFAttributes <- function(
    file,
    select = c("gene_", "transcript_"),
    unique = TRUE,
    return = c("data.frame", "list")
) {
    gff <- readGFF(file)
    assert_is_character(select)
    assert_is_a_bool(unique)
    return <- match.arg(return)

    inform("Parsing GFF key value pairs")
    strings <- gff %>%
        .[["keyValuePairs"]] %>%
        as.character()

    # This can be significantly faster for large GFF files
    if (isTRUE(unique)) {
        inform("Getting unique attributes")
        strings <- unique(strings)
    }

    # Require selected keys (partial matching is supported)
    hits <- vapply(
        X = strings,
        FUN = function(string) {
            all(vapply(
                X = select,
                FUN = function(key) {
                    grepl(key, string, fixed = TRUE)
                },
                FUN.VALUE = logical(1L)
            ))
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
    strings <- strings[hits]

    inform(paste(length(strings), "key value pairs"))

    # This can take a long time for large genomes, so use a progress bar
    list <- pblapply(strings, function(x) {
        # Remove trailing delim
        x <- gsub(
            pattern = ";$",
            replacement = "",
            x = x,
            fixed = FALSE
        )
        # Remove space after delim
        x <- gsub(
            pattern = "; ",
            replacement = ";",
            x = x,
            fixed = TRUE
        )
        # Set `=` as the keypair delim. Ensembl uses ` `
        x <- gsub(
            pattern = " ",
            replacement = "=",
            x = x,
            fixed = TRUE
        )
        # Now ready to split
        list <- strsplit(
            x = x,
            split = ";",
            fixed = TRUE
        )
        # Break out the key value pairs
        match <- str_match(
            string = unlist(list),
            pattern = "^([^=]+)=([^=]+)$"
        )
        # Define the key value pairs
        key <- match[, 2L]
        value <- match[, 3L]
        names(value) <- key

        # Only return values that match `select` argument
        grepl <- grepl(
            pattern = paste(keys, collapse = "|"),
            x = names(value)
        )
        value[grepl]
    })

    if (return == "data.frame") {
        ldply(list, rbind) %>%
            # Return strings instead of factors
            mutate_if(is.factor, as.character)
    } else if (return == "list") {
        list
    }
}
