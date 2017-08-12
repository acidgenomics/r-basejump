# tibble ====
.asTibble <- function(from) {
    if (is.null(dim(from))) {
        stop("Object must support 'dim'")
    }
    from <- as.data.frame(from)
    if (has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    tibble::as_tibble(from)
}

setAs("ANY", "tbl_df", .asTibble)
setAs("ANY", "tibble", .asTibble)
