#' @inherit Gene2Symbol-class
#'
#' @note
#' - This function will make any duplicated symbols unique by applying
#'   [base::make.unique()], which will add ".1" to the end of the gene name.
#' - No attempt is made to arrange the rows by gene identifier.
#'
#' @name Gene2Symbol
#'
#' @inheritParams general
#' @param sanitization `string`. Sanitization method to apply:
#' - `makeUnique`: Apply [base::make.unique()] to the `geneName` column.
#'   Gene symbols are made unique, while the gene IDs remain unmodified.
#' - `1:1`: For gene symbols that map to multiple gene IDs, select only
#'   the first annotated gene ID.
#' - `unmodified`: Return both the `geneID` and `geneName` columns unmodified.
#'   For some organisms (e.g. *Homo sapiens* and *Mus musculus*), gene names
#'   and gene symbols do not map 1:1, so this is not generally recommended.
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' data(rse_small)
#' x <- Gene2Symbol(rse_small)
#' print(x)
NULL



Gene2Symbol.DataFrame <-  # nolint
    function(
        object,
        sanitization = c("makeUnique", "1:1", "none")
    ) {
        assert_has_rows(object)
        sanitization <- match.arg(sanitization)

        # Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object %>%
            # Perform this first, otherwise can get a non atomic error due to
            # GRanges to DataFrame coercion containing "X" ranges column.
            .[, cols, drop = FALSE] %>%
            as_tibble(rownames = NULL) %>%
            # This step is needed for handling raw GFF annotations.
            unique() %>%
            mutate_all(as.character) %>%
            arrange(!!!syms(cols))

        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(paste(
                length(dupes), "non-unique gene symbols detected."
            ))
            if (sanitization == "makeUnique") {
                message("Making gene symbols unique.")
                data <- mutate(
                    data,
                    !!sym("geneName") := make.unique(!!sym("geneName"))
                )
            } else if (sanitization == "1:1") {
                message("Making 1:1 using oldest gene ID.")
                data <- data %>%
                    arrange(!!!syms(cols)) %>%
                    group_by(!!sym("geneName")) %>%
                    slice(n = 1L) %>%
                    ungroup() %>%
                    arrange(!!!syms(cols))
            } else {
                # Warn if there are duplicates that won't be sanitized.
                if (any(duplicated(object[["geneName"]]))) {
                    warning(paste(
                        "Object contains duplicate gene symbols.",
                        "Sanitization is recommended.",
                        sep = "\n"
                    ))
                }
            }
        }

        data <- as(data, "DataFrame")
        metadata(data) <- .genomeMetadata(object)
        metadata(data)[["sanitization"]] <- sanitization
        new(Class = "Gene2Symbol", data)
    }



Gene2Symbol.GRanges <-  # nolint
    function(object, sanitization) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                sanitization = sanitization
            )
        )
    }
formals(Gene2Symbol.GRanges) <- formals(Gene2Symbol.DataFrame)



Gene2Symbol.SummarizedExperiment <-  # nolint
    function(object, sanitization) {
        validObject(object)
        rownames <- rownames(object)
        if (is(object, "RangedSummarizedExperiment")) {
            data <- rowRanges(object)
        } else {
            data <- rowData(object)
        }
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                sanitization = sanitization
            )
        )
    }
formals(Gene2Symbol.SummarizedExperiment) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = Gene2Symbol.DataFrame
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = Gene2Symbol.GRanges
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = Gene2Symbol.SummarizedExperiment
)
