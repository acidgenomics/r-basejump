#' Percentage of Zeros vs. Library Depth
#'
#' Calculate and visualize the dropout rate.
#'
#' @name zerosVsDepth
#' @author Rory Kirchner, Michael Steinbaugh
#' @inheritParams params
#'
#' @return `matrix`.
#'
#' @examples
#' data(sce)
#' x <- zerosVsDepth(sce)
#' summary(x)
#' colnames(x)
NULL



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



# Using a logical matrix is faster and more memory efficient.
# Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
zerosVsDepth.sparseMatrix <-  # nolint
    function(object) {
        assert(is(object, "sparseMatrix"))
        assert(!is(object, "lgCMatrix"))
        present <- object %>%
            as("dgCMatrix") %>%
            as("lgCMatrix")
        colSums <- Matrix::colSums
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



zerosVsDepth.SummarizedExperiment <-  # nolint
    function(object, assay = 1L) {
        assertScalar(assay)
        counts <- assays(object)[[assay]]
        data <- zerosVsDepth(counts)
        sampleData <- sampleData(object)
        assertIdentical(rownames(data), rownames(sampleData))
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



zerosVsDepth.SingleCellExperiment <-  # nolint
    function(object, assay = 1L) {
        assertScalar(assay)
        counts <- assays(object)[[assay]]

        data <- zerosVsDepth(counts)
        data[["sampleID"]] <- cell2sample(object)

        sampleData <- sampleData(object)
        sampleData[["sampleID"]] <- as.factor(rownames(sampleData))

        assertClass(data, "DataFrame")
        assertClass(sampleData, "DataFrame")

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
