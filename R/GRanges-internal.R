.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

    # Ensure GRanges is sorted by names
    message("Sorting GRanges by identifier")
    object <- object[sort(names(object))]

    # Standardize the metadata columns
    message("Standardizing the metadata columns")
    mcols <- mcols(object)
    # Sanitize to camel case
    mcols <- camel(mcols)
    # Ensure "ID" is always capitalized (e.g. "entrezid")
    colnames(mcols) <- gsub("id$", "ID", colnames(mcols))
    # Use `transcript` instead of `tx` consistently
    colnames(mcols) <- gsub(
        pattern = "^tx",
        replacement = "transcript",
        x = colnames(mcols)
    )
    # Remove columns that are all NA
    mcols <- removeNA(mcols)

    # Missing `geneName`
    if (!"geneName" %in% colnames(mcols)) {
        warning(paste(
            "`geneName` is missing.",
            "Using `geneID` in place."
        ))
        assert_is_subset("geneID", colnames(mcols))
        mcols[["geneName"]] <- mcols[["geneID"]]
    }

    # Missing `transcriptName`
    if (
        "transcriptID" %in% colnames(mcols) &&
        !"transcriptName" %in% colnames(mcols)
    ) {
        warning(paste(
            "`transcriptName` is missing.",
            "Using `transcriptID` in place."
        ))
        mcols[["transcriptName"]] <- mcols[["transcriptID"]]
    }

    # Always use `geneName` instead of `symbol`
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        message("Using `geneName` instead of `symbol`")
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        # nocov start
        message("Renaming `symbol` to `geneName`")
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
        # nocov end
    }

    # Sanitize any character columns that have duplicates into factor
    message("Converting strings to factors")
    mcols <- lapply(
        X = mcols,
        FUN = function(col) {
            if (is.character(col) && any(duplicated(col))) {
                as.factor(col)
            } else {
                col
            }
        }
    )
    mcols <- as(mcols, "DataFrame")

    # Require that names match the identifier column
    # Check `transcriptID` then `geneID`
    assert_are_intersecting_sets(annotationCols, colnames(mcols))
    if ("transcriptID" %in% colnames(mcols)) {
        idCol <- "transcriptID"
    } else {
        idCol <- "geneID"
    }
    assert_are_identical(names(object), mcols[[idCol]])
    mcols(object) <- mcols

    # Ensure broad class definitions are included
    mcols(object)[["broadClass"]] <- broadClass(object)

    # Sort metadata columns alphabetically
    mcols(object) <- mcols(object)[, sort(colnames(mcols(object)))]

    assert_is_all_of(object, "GRanges")
    object
}
