#' Melt Count Matrix to Long Format
#'
#' @name meltCounts
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param nonzeroGenes `boolean`. Return only non-zero genes.
#' @param trans `string`. Apply a log transformation (e.g. `log2(x + 1L)`) to
#'   the count matrix prior to melting, if desired. Use `"identity"` to return
#'   unmodified (default).
#'
#' @seealso [reshape2::melt()].
#'
#' @return `grouped_df`, grouped by `colname` (e.g. sample ID) and `rowname`
#'   (e.g. gene ID).
#'
#' @examples
#' data(rse_small)
#' x <- meltCounts(rse_small)
#' print(x)
NULL



.meltCounts.SE <-  # nolint
    function(
        object,
        assay = 1L,
        nonzeroGenes = FALSE,
        trans = c("identity", "log2", "log10")
    ) {
        validObject(object)
        assert_is_scalar(assay)
        assert_is_a_bool(nonzeroGenes)
        trans <- match.arg(trans)

        # Prepare the count matrix.
        counts <- assays(object)[[assay]]
        assert_is_non_empty(counts)
        # Coerce to dense matrix.
        counts <- as.matrix(counts)

        # Remove genes with all zero counts.
        if (isTRUE(nonzeroGenes)) {
            keep <- rowSums(counts) > 0L
            counts <- counts[keep, , drop = FALSE]
            message(paste(nrow(counts), "non-zero genes detected."))
        }

        # Log transform the matrix, if desired.
        if (trans != "identity") {
            message(paste0("Applying ", trans, "(x + 1) transformation..."))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert_is_function(fun)
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
    definition = .meltCounts.SE
)
