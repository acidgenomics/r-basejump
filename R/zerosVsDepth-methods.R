#' @name zerosVsDepth
#' @author Rory Kirchner, Michael Steinbaugh
#' @inherit bioverbs::zerosVsDepth
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(sce, package = "acidtest")
#' x <- zerosVsDepth(sce)
#' summary(x)
#' colnames(x)
NULL



#' @rdname zerosVsDepth
#' @name zerosVsDepth
#' @importFrom bioverbs zerosVsDepth
#' @usage zerosVsDepth(object, ...)
#' @export
NULL



zerosVsDepth.matrix <-  # nolint
    function(object) {
        present <- object > 0L
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = as.integer(colSums(object)),
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
            depth = as.integer(colSums(object)),
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
        assert(isScalar(assay))
        counts <- assays(object)[[assay]]
        data <- zerosVsDepth(counts)
        sampleData <- sampleData(object)
        assert(
            identical(rownames(data), rownames(sampleData)),
            areDisjointSets(colnames(data), colnames(sampleData))
        )
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
        assert(isScalar(assay))
        counts <- assays(object)[[assay]]

        data <- zerosVsDepth(counts)
        data[["sampleID"]] <- cell2sample(object)

        sampleData <- sampleData(object)
        sampleData[["sampleID"]] <- as.factor(rownames(sampleData))

        assert(
            is(data, "DataFrame"),
            is(sampleData, "DataFrame")
        )
        # Consider using BiocTibble approach here in a future update.
        join <- left_join(
            x = as_tibble(data, rownames = "rowname"),
            y = as_tibble(sampleData, rownames = NULL),
            by = "sampleID"
        )

        out <- as(join, "DataFrame")
        assert(hasRownames(out))
        out
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("SingleCellExperiment"),
    definition = zerosVsDepth.SingleCellExperiment
)
