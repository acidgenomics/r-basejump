.sanitizeAnnotationCols <- function(object) {
    # Get mcols, if necessary
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }

    # Rename the columns
    colnames(data) <- colnames(data) %>%
        camel() %>%
        # Ensure "ID" is capitalized (e.g. entrezid)
        gsub("id$", "ID", .)

    # Always use `geneID` instead of `symbol`
    if (all(c("geneID", "symbol") %in% colnames(data))) {
        inform("Using `geneID` instead of `symbol`")
        data[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(data)) {
        inform("Renaming `symbol` to `geneID`")
        data[["geneID"]] <- data[["symbol"]]
        data[["symbol"]] <- NULL
    }

    assert_are_intersecting_sets(annotationCols, colnames(data))
    priorityCols <- intersect(annotationCols, colnames(data))

    data <- data %>%
        .[, unique(c(priorityCols, colnames(.))), drop = FALSE]

    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }

    object
}
