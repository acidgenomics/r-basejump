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

    # Always use geneName instead of symbol
    if (all(c("geneName", "symbol") %in% colnames(data))) {
        inform("Using geneName instead of symbol")
        data[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(data)) {
        inform("Renaming symbol to geneName")
        data[["geneName"]] <- data[["symbol"]]
        data[["symbol"]] <- NULL
    }

    # Set strings as factors
    if (is.character(data[["geneBiotype"]])) {
        inform("Setting geneBiotype as factor")
        data[["geneBiotype"]] <- as.factor(data[["geneBiotype"]])
    }
    if (is.character(data[["seqName"]])) {
        inform("Setting seqName as factor")
        data[["seqName"]] <- as.factor(data[["seqName"]])
    }
    if (is.integer(data[["seqStrand"]])) {
        inform("Setting seqStrand as factor")
        data[["seqStrand"]] <- as.factor(data[["seqStrand"]])
    }
    if (is.character(data[["txBiotype"]])) {
        inform("Setting txBiotype as factor")
        data[["txBiotype"]] <- as.factor(data[["txBiotype"]])
    }
    if (is.integer(data[["txSupportLevel"]])) {
        inform("Setting txSupportLevel as factor")
        data[["txSupportLevel"]] <- as.factor(data[["txSupportLevel"]])
    }

    # Put the priority columns first
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
