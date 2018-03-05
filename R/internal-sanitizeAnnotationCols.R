.sanitizeAnnotationCols <- function(
    object,
    format = c("genes", "gene2symbol", "transcripts", "tx2gene")
) {
    format <- match.arg(format)

    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }

    # Rename the columns
    colnames(data) <- colnames(data) %>%
        camel() %>%
        gsub("^entrezid", "entrez", .) %>%
        gsub("^geneID$", "ensgene", .) %>%
        gsub("^geneName$", "symbol", .) %>%
        gsub("^(gene|tx)Biotype", "biotype", .) %>%
        gsub("^txID$", "enstxp", .)

    # Reorder priority columns for genes and transcripts
    if (format %in% c("genes", "transcripts")) {
        if (format == "genes") {
            priorityCols <- geneAnnotationCols
        } else if (format == "transcripts") {
            priorityCols <- transcriptAnnotationCols
        }
        # Add the `broadClass` column, if present
        if ("broadClass" %in% colnames(data)) {
            priorityCols <- c(priorityCols, "broadClass")
        }
        assert_are_intersecting_sets(priorityCols, colnames(data))
        priorityCols <- intersect(priorityCols, colnames(data))
        data <- data %>%
            .[, c(priorityCols, setdiff(colnames(.), priorityCols)),
                drop = FALSE]
    }

    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }

    object
}
