#' Melt Counts Matrix to Long Format
#'
#' @name meltCounts
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param trans `string`. Apply a log transformation (e.g. `log2(x + 1L)`) to
#'   the counts matrix prior to melting, if desired. Use `"identity"` to return
#'   unmodified (default).
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
        trans = c("identity", "log2", "log10")
    ) {
        validObject(object)
        trans <- match.arg(trans)

        # Prepare the counts matrix.
        counts <- assay(object)
        assert_is_non_empty(counts)
        # Coerce to dense matrix.
        counts <- as.matrix(counts)

        # Log transform the matrix, if desired.
        if (trans != "identity") {
            message(paste0("Applying", trans, "(x + 1) transformation"))
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
