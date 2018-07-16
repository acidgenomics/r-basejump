.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

    # Sanitize mcols to camel case
    object <- camel(object)

    # Use `transcript` instead of `tx` consistently
    colnames(mcols(object)) <- gsub(
        pattern = "^tx",
        replacement = "transcript",
        x = colnames(mcols(object))
    )

    # Ensure GRanges is sorted by names
    object <- object[sort(names(object))]

    # Standardize the metadata columns
    message("Standardizing the metadata columns")
    mcols <- mcols(object)
    # Remove columns that are all NA
    mcols <- removeNA(mcols)
    # Rename the columns
    colnames(mcols) <- colnames(mcols) %>%
        camel() %>%
        # Ensure "ID" is capitalized (e.g. entrezid)
        gsub("id$", "ID", .)

    # Always use geneName instead of symbol
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        message("Using geneName instead of symbol")
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        # nocov start
        message("Renaming symbol to geneName")
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
        # nocov end
    }

    # Set strings as factors
    if (is.character(mcols[["geneBiotype"]])) {
        message("Setting geneBiotype as factor")
        mcols[["geneBiotype"]] <- as.factor(mcols[["geneBiotype"]])
    }
    if (is.character(mcols[["seqCoordSystem"]])) {
        message("Setting seqCoordSystem as factor")
        mcols[["seqCoordSystem"]] <- as.factor(mcols[["seqCoordSystem"]])
    }
    if (is.character(mcols[["txBiotype"]])) {
        message("Setting txBiotype as factor")
        mcols[["txBiotype"]] <- as.factor(mcols[["txBiotype"]])
    }
    if (is.integer(mcols[["txSupportLevel"]])) {
        message("Setting txSupportLevel as factor")
        mcols[["txSupportLevel"]] <- as.factor(mcols[["txSupportLevel"]])
    }

    # Put the priority columns first
    assert_are_intersecting_sets(annotationCols, colnames(mcols))
    priorityCols <- intersect(annotationCols, colnames(mcols))
    mcols <- mcols %>%
        .[, unique(c(priorityCols, colnames(.))), drop = FALSE]
    mcols(object) <- mcols

    # Require that the first column contains the names (e.g. `geneID`)
    assert_are_identical(names(object), mcols(object)[[1L]])

    # Add broad class definitions
    mcols(object)[["broadClass"]] <- broadClass(object)

    assert_is_all_of(object, "GRanges")
    object
}
