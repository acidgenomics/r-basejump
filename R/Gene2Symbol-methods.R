#' @name Gene2Symbol
#' @inherit Gene2Symbol-class
#' @inheritParams params
#'
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"makeUnique"`: *Recommended.* Apply `make.unique` to the `geneName`
#'     column. Gene symbols are made unique, while the gene IDs remain
#'     unmodified.
#'   - `"1:1"`: For gene symbols that map to multiple gene IDs, select only the
#'     first annotated gene ID.
#'   - `"long"`: Return `geneID` and `geneName` columns unmodified in long
#'     format.
#'
#' @examples
#' data(rse)
#' x <- Gene2Symbol(rse)
#' print(x)
NULL



Gene2Symbol.DataFrame <-  # nolint
    function(object, format = c("makeUnique", "1:1", "long")) {
        assert(hasRows(object))
        format <- match.arg(format)

        # Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            geneName = as.character(decode(object[["geneName"]])),
            row.names = rownames(object)
        )

        # Inform the user about how many symbols multi-map.
        # Note that `duplicated` doesn't work on Rle, so we have to coerce
        # columns to character first (see `as_tibble` call above).
        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(paste(
                length(dupes), "non-unique gene symbol(s) detected."
            ))
        }

        if (format == "makeUnique") {
            # Returning 1:1 mappings with renamed gene symbols.
            # This is the default, and including a message is too noisy, since
            # it is used heavily in other functions.
            data[["geneName"]] <- make.unique(data[["geneName"]])
        } else if (format == "1:1") {
            message("Returning 1:1 mappings using oldest gene ID per symbol.")
            data <- data %>%
                as_tibble(rownames = NULL) %>%
                group_by(!!sym("geneName")) %>%
                arrange(!!!sym("geneID"), .by_group = TRUE) %>%
                slice(n = 1L) %>%
                ungroup()
        } else if (format == "long") {
            message("Returning 1:many in long format (not recommended).")
        }

        data <- as(data, "DataFrame")
        metadata(data) <- .slotGenomeMetadata(object)
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



Gene2Symbol.GRanges <-  # nolint
    function(object, format) {
        data <- as(object, "DataFrame")
        data <- unique(data)
        metadata(data) <- metadata(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(Gene2Symbol.GRanges) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = Gene2Symbol.GRanges
)



Gene2Symbol.SummarizedExperiment <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(Gene2Symbol.SummarizedExperiment) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = Gene2Symbol.SummarizedExperiment
)
