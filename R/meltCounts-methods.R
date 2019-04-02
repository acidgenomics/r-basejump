#' @name meltCounts
#' @inherit bioverbs::meltCounts
#' @inheritParams params
#'
#' @param nonzeroGenes `logical(1)`.
#'   Return only non-zero genes.
#' @param trans `character(1)`.
#'   Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#'   to melting, if desired. Use `"identity"` to return unmodified (default).
#'
#' @examples
#' data(rse, package = "acidtest")
#' x <- meltCounts(rse)
#' print(x)
NULL



#' @rdname meltCounts
#' @name meltCounts
#' @importFrom bioverbs meltCounts
#' @export
NULL



meltCounts.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        nonzeroGenes = FALSE,
        trans = c("identity", "log2", "log10")
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(nonzeroGenes)
        )
        trans <- match.arg(trans)

        # Prepare the count matrix.
        counts <- assays(object)[[assay]]
        assert(hasLength(counts))
        # Always coerce to dense matrix prior to melting.
        counts <- as.matrix(counts)

        # Remove genes with all zero counts.
        if (isTRUE(nonzeroGenes)) {
            keep <- rowSums(counts) > 0L
            counts <- counts[keep, , drop = FALSE]
            message(paste(nrow(counts), "non-zero genes detected."))
        }

        # Log transform the matrix, if desired.
        if (trans != "identity") {
            message(paste0("Applying ", trans, "(x + 1) transformation."))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert(is.function(fun))
            counts <- fun(counts + 1L)
        }

        # Get the sample metadata.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "rowname") %>%
            rename(colname = !!sym("rowname")) %>%
            mutate_all(as.factor)

        # Return as melted tibble.
        counts %>%
            # Using reshape2 method here.
            # This sets rownames as "Var1" and colnames as "Var2".
            melt(id = 1L, value.name = "counts") %>%
            as_tibble() %>%
            rename(
                rowname = !!sym("Var1"),
                colname = !!sym("Var2")
            ) %>%
            mutate_if(is.character, as.factor) %>%
            group_by(!!!syms(c("colname", "rowname"))) %>%
            left_join(sampleData, by = "colname")
    }



#' @rdname meltCounts
#' @export
setMethod(
    f = "meltCounts",
    signature = signature("SummarizedExperiment"),
    definition = meltCounts.SummarizedExperiment
)
