#' Melt Counts Matrix to Long Format
#'
#' @name meltCounts
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param log2Transform `boolean`. Apply `log2(x + 1L)` transformation to the
#'   counts matrix before melt?
#'
#' @seealso [reshape2::melt()].
#'
#' @return `grouped_df`, grouped by `colname` (e.g. sample ID) and `rowname`
#'   (e.g. gene ID).
#'
#' @examples
#' x <- meltCounts(rse_small)
#' print(x)
NULL



#' @rdname meltCounts
#' @export
setMethod(
    f = "meltCounts",
    signature = signature("SummarizedExperiment"),
    definition = function(
        object,
        log2Transform = FALSE
    ) {
        validObject(object)
        assert_is_a_bool(log2Transform)

        # Prepare the counts matrix.
        counts <- assay(object)
        assert_is_non_empty(counts)
        # Coerce to dense matrix.
        counts <- as.matrix(counts)

        # log2 transform the matrix, if desired.
        if (isTRUE(log2Transform)) {
            counts <- log2(counts + 1L)
        }

        # Get the sample metadata.
        sampleData <- sampleData(object) %>%
            as("tbl_df") %>%
            rename(colname = !!sym("rowname")) %>%
            mutate_all(as.factor)

        # Return as melted tibble.
        counts %>%
            # Using reshape2 method here.
            # This sets rownames as "Var1" and colnames as "Var2".
            melt(id = 1L, value.name = "counts") %>%
            as("tbl_df") %>%
            rename(
                rowname = !!sym("Var1"),
                colname = !!sym("Var2")
            ) %>%
            mutate_if(is.character, as.factor) %>%
            group_by(!!!syms(c("colname", "rowname"))) %>%
            left_join(sampleData, by = "colname")
    }
)
