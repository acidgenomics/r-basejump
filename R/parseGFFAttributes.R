#' Parse GFF Attributes Column
#'
#' @note Memory overhead and speed is greatly improved by requiring keys.
#' Deparsing only unique values is much faster and generally recommended.
#'
#' @inheritParams general
#' @param select Attribute to select. Supports partial matching. Defaults to
#'   returning attributes matching both "`gene_`" and "`transcript_`".
#' @param unique Return unique attributes.
#'
#' @return `tbl_df`.
#' @export
#'
#' @examples
#' parseGFFAttributes("http://basejump.seq.cloud/mmusculus.gtf") %>%
#'     glimpse()
parseGFFAttributes <- function(
    file,
    select = c("gene_", "transcript_"),
    unique = TRUE
) {
    gff <- readGFF(file)
    assert_is_character(select)
    assert_is_a_bool(unique)

    strings <- as.character(gff[["attribute"]])

    if (isTRUE(unique)) {
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
    assert_is_non_empty(strings)

    if (isTRUE(unique)) {
        strings <- unique(strings)
    }

    # Show a progress bar when parsing a large number of strings
    if (length(strings) > 10000L) {
        fxn <- pbapply
    } else {
        fxn <- lapply
    }
    list <- fxn(strings, function(x) {
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
            pattern = paste(select, collapse = "|"),
            x = names(value)
        )
        value[grepl]
    })

    data <- ldply(list, rbind) %>%
        as_tibble() %>%
        # Return strings instead of factors
        mutate_all(as.character) %>%
        # Ensure columns always return sorted
        .[, sort(colnames(.))]

    if (isTRUE(unique)) {
        data <- unique(data)
    }

    data
}
