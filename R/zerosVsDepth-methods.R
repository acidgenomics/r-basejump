#' @name zerosVsDepth
#' @author Rory Kirchner, Michael Steinbaugh
#' @inherit bioverbs::zerosVsDepth
#' @note Updated 2019-08-11.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#' sce <- SingleCellExperiment
#'
#' ## SingleCellExperiment ====
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



## Updated 2019-07-22.
`zerosVsDepth,matrix` <-  # nolint
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
    definition = `zerosVsDepth,matrix`
)



## Using a logical matrix is faster and more memory efficient.
## Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
## Updated 2019-08-11.
`zerosVsDepth,sparseMatrix` <-  # nolint
    function(object) {
        assert(is(object, "sparseMatrix"))
        assert(!is(object, "lgCMatrix"))
        present <- as(object, "dgCMatrix")
        present <- as(present, "lgCMatrix")
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
    definition = `zerosVsDepth,sparseMatrix`
)



## Updated 2019-08-06.
`zerosVsDepth,SummarizedExperiment` <-  # nolint
    function(object, assay = 1L) {
        assert(isScalar(assay))
        counts <- assay(object, i = assay)
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
    definition = `zerosVsDepth,SummarizedExperiment`
)



## Updated 2019-08-11.
`zerosVsDepth,SingleCellExperiment` <-  # nolint
    function(object, assay = 1L) {
        assert(isScalar(assay))
        counts <- assay(object, i = assay)
        data <- zerosVsDepth(counts)
        data[["sampleID"]] <- cell2sample(object)
        sampleData <- sampleData(object)
        sampleData[["sampleID"]] <- as.factor(rownames(sampleData))
        assert(
            is(data, "DataFrame"),
            is(sampleData, "DataFrame")
        )
        out <- left_join(x = data, y = sampleData, by = "sampleID")
        assert(
            is(out, "DataFrame"),
            hasRownames(out)
        )
        out
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("SingleCellExperiment"),
    definition = `zerosVsDepth,SingleCellExperiment`
)
