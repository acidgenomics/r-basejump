# tibble ====
.asTibble <- function(from) {
    from <- as.data.frame(from)
    if (has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    tibble::as_tibble(from)
}

setAs("data.frame", "tibble", .asTibble)
setAs("DataFrame", "tibble", .asTibble)
setAs("matrix", "tibble", .asTibble)
setAs("tbl_df", "tibble", .asTibble)  # Ensure rownames to column
