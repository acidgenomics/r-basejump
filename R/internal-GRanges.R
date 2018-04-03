#' Detect ID Column
#'
#' Utility function for automatically setting rownames. Note that the transcript
#' ID column takes priority over the gene ID column.
#'
#' @keywords internal
#' @noRd
.detectIDCol <- function(object) {
    object <- as.data.frame(object)
    assert_are_intersecting_sets(
        x = c("txID", "geneID"),
        y = colnames(object)
    )
    txCol <- match("txID", colnames(object)) %>%
        na.omit()
    geneCol <- match("geneID", colnames(object)) %>%
        na.omit()
    if (length(txCol)) {
        index <- txCol[[1L]]
    } else {
        index <- geneCol[[1L]]
    }
    colnames(object)[[index]]
}



.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

    # Ensure GRanges is sorted by names
    object <- object[sort(names(object))]

    # Standardize the metadata columns
    object <- .standardizeGRangesMetadata(object)
    assert_are_identical(names(object), mcols(object)[[1L]])

    # Add broad class definitions
    mcols(object)[["broadClass"]] <- broadClass(object)

    assert_is_all_of(object, "GRanges")
    object
}



.standardizeGRangesMetadata <- function(object) {
    assert_is_all_of(object, "GRanges")
    inform("Standardizing the metadata columns")
    data <- mcols(object)

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
    if (is.character(data[["seqCoordSystem"]])) {
        inform("Setting seqCoordSystem as factor")
        data[["seqCoordSystem"]] <- as.factor(data[["seqCoordSystem"]])
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

    mcols(object) <- data
    object
}
