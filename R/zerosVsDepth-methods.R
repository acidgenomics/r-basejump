#' @name zerosVsDepth
#' @inherit basejump.generics::zerosVsDepth
#' @inheritParams params
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @examples
#' data(sce)
#' x <- zerosVsDepth(sce)
#' summary(x)
#' colnames(x)
NULL



#' @importFrom basejump.generics zerosVsDepth
#' @aliases NULL
#' @export
basejump.generics::zerosVsDepth



# matrix =======================================================================
zerosVsDepth.matrix <-  # nolint
    function(object) {
        present <- object > 0L
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("matrix"),
    definition = zerosVsDepth.matrix
)



# sparseMatrix =================================================================
# Using a logical matrix is faster and more memory efficient.
# Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
zerosVsDepth.sparseMatrix <-  # nolint
    function(object) {
        assert_that(is(object, "sparseMatrix"))
        assert_that(!is(object, "lgCMatrix"))
        present <- object %>%
            as("dgCMatrix") %>%
            as("lgCMatrix")
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("sparseMatrix"),
    definition = zerosVsDepth.sparseMatrix
)



# SummarizedExperiment =========================================================
zerosVsDepth.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L
    ) {
        assert_is_scalar(assay)
        counts <- assays(object)[[assay]]
        data <- zerosVsDepth(counts)
        sampleData <- sampleData(object)
        assert_are_identical(rownames(data), rownames(sampleData))
        assert_are_disjoint_sets(colnames(data), colnames(sampleData))
        cbind(data, sampleData)
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = zerosVsDepth.SummarizedExperiment
)



# SingleCellExperiment =========================================================
zerosVsDepth.SingleCellExperiment <-  # nolint
    function(
        object,
        assay = 1L
    ) {
        assert_is_scalar(assay)
        counts <- assays(object)[[assay]]

        data <- zerosVsDepth(counts)
        data[["sampleID"]] <- cell2sample(object)

        sampleData <- sampleData(object)
        sampleData[["sampleID"]] <- as.factor(rownames(sampleData))

        assert_is_all_of(data, "DataFrame")
        assert_is_all_of(sampleData, "DataFrame")

        # Use BiocTibble left_join DataFrame method here.
        join <- left_join(
            x = as_tibble(data, rownames = "rowname"),
            y = as_tibble(sampleData, rownames = NULL),
            by = "sampleID"
        )
        out <- as(join, "DataFrame")
        assertHasRownames(out)
        out
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("SingleCellExperiment"),
    definition = zerosVsDepth.SingleCellExperiment
)
