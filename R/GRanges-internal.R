# Report the source of the gene annotations
.gffSource <- function(gff) {
    assert_is_subset("source", colnames(mcols(gff)))
    if (any(grepl("FlyBase", mcols(gff)[["source"]]))) {
        "FlyBase"  # nocov
    } else if (any(grepl("WormBase", mcols(gff)[["source"]]))) {
        "WormBase"  # nocov
    } else if (any(grepl(
        "ensembl", mcols(gff)[["source"]], ignore.case = TRUE
    ))) {
        "Ensembl"
    } else {
        stop("Unsupported GFF source")  # nocov
    }
}



# Determine if GFF or GTF
.gffType <- function(gff) {
    stopifnot(is(gff, "GRanges"))
    gff <- camel(gff)
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        "GFF"
    } else {
        "GTF"
    }
}



.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

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
        # nocov start
        warning(paste(
            "`geneName` is missing.",
            "Using `geneID` in place."
        ))
        assert_is_subset("geneID", colnames(mcols))
        mcols[["geneName"]] <- mcols[["geneID"]]
        # nocov end
    }

    # Missing `transcriptName`
    if (
        "transcriptID" %in% colnames(mcols) &&
        !"transcriptName" %in% colnames(mcols)
    ) {
        # nocov start
        warning(paste(
            "`transcriptName` is missing.",
            "Using `transcriptID` in place."
        ))
        mcols[["transcriptName"]] <- mcols[["transcriptID"]]
        # nocov end
    }

    # Always use `geneName` instead of `symbol`
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        # nocov start
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
        # nocov end
    }

    # Sanitize any character columns that have duplicates into factor
    mcols <- lapply(
        X = mcols,
        FUN = function(col) {
            if (is.character(col) && any(duplicated(col))) {
                as.factor(col)
            } else {
                # `I` inhibits reinterpretation and returns AsIs
                # Recommended in the DataFrame documentation
                I(col)
            }
        }
    )
    mcols <- as(mcols, "DataFrame")
    mcols(object) <- mcols

    # Require that names match the identifier column
    # Use `transcriptID` over `geneID` if defined
    assert_are_intersecting_sets(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(object))
    )
    if ("transcriptID" %in% colnames(mcols(object))) {
        idCol <- "transcriptID"
    } else {
        idCol <- "geneID"
    }
    names(object) <- mcols(object)[[idCol]]

    # Ensure broad class definitions are included
    mcols(object)[["broadClass"]] <- broadClass(object)

    # Sort metadata columns alphabetically
    mcols(object) <- mcols(object)[, sort(colnames(mcols(object)))]

    # Ensure GRanges is sorted by names
    message(paste("Sorting ranges by", idCol))
    object <- object[sort(names(object))]

    assert_is_all_of(object, "GRanges")
    object
}
