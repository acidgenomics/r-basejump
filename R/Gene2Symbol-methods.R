#' @inherit Gene2Symbol-class
#'
#' @note For some organisms, gene names and gene symbols do not map 1:1
#' (e.g. *Homo sapiens* and *Mus musculus*). Refer to the `format` argument here
#' in the documentation for approaches that deal with this issue.
#'
#' @name Gene2Symbol
#'
#' @inheritParams general
#' @param format `string`. Formatting method to apply:
#' - `"makeUnique"`: *Recommended.* Apply [base::make.unique()] to the
#'   `geneName` column. Gene symbols are made unique, while the gene IDs remain
#'   unmodified.
#' - `"1:1"`: For gene symbols that map to multiple gene IDs, select only the
#'   first annotated gene ID.
#' - `"long"`: Return `geneID` and `geneName` columns unmodified in long format.
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' data(rse_small)
#' x <- Gene2Symbol(rse_small)
#' print(x)
NULL



# DataFrame ====================================================================
Gene2Symbol.DataFrame <-  # nolint
    function(
        object,
        format = c("makeUnique", "1:1", "long")
    ) {
        assert_has_rows(object)
        format <- match.arg(format)

        # Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object[, cols, drop = FALSE]

        # Inform the user about how many symbols multi-map.
        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(paste(
                length(dupes), "non-unique gene symbols detected."
            ))
        }

        if (format == "makeUnique") {
            message("Returning 1:1 mappings with renamed gene symbols.")
            data[["geneName"]] <- data[["geneName"]] %>%
                as.character() %>%
                make.unique()
        } else if (format == "1:1") {
            message("Returning 1:1 mappings using oldest gene ID per symbol.")
            data <- data %>%
                as_tibble(rownames = NULL) %>%
                mutate_all(as.character) %>%
                group_by(!!sym("geneName")) %>%
                arrange(!!!sym("geneID"), .by_group = TRUE) %>%
                slice(n = 1L) %>%
                ungroup()
        } else if (format == "long") {
            message("Returning mappings in long format.")
        }

        data <- as(data, "DataFrame")
        metadata(data) <- .genomeMetadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Gene2Symbol", data)
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = Gene2Symbol.DataFrame
)



# GRanges ======================================================================
Gene2Symbol.GRanges <-  # nolint
    function(object, format) {
        data <- as(object, "DataFrame")
        data <- unique(data)
        metadata(data) <- metadata(object)
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                format = format
            )
        )
    }
formals(Gene2Symbol.GRanges) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = Gene2Symbol.GRanges
)



# SummarizedExperiment =========================================================
Gene2Symbol.SummarizedExperiment <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                format = format
            )
        )
    }
formals(Gene2Symbol.SummarizedExperiment) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = Gene2Symbol.SummarizedExperiment
)
