#' @name melt
#' @inherit AcidPlyr::melt
#'
#' @note Updated 2020-10-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dim(object)
#' x <- melt(object)
#' nrow(x)
#' print(x)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' dim(object)
#' x <- melt(object)
#' nrow(x)
#' print(x)
NULL



## Updated 2020-10-07.
`melt,matrix` <-  # nolint
    methodFunction(
        f = "melt",
        signature = "matrix",
        package = "AcidPlyr"
    )



## Updated 2019-08-26.
`melt,Matrix` <-  # nolint
    appendToBody(
        fun = `melt,matrix`,
        values = quote(rowSums <- Matrix::rowSums)
    )



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("Matrix"),
    definition = `melt,Matrix`
)



## Updated 2019-08-24.
`melt,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        min,
        minMethod,
        trans
    ) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        counts <- assay(object, i = assay)
        data <- melt(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        colnamesCol <- colnames(data)[[2L]]
        colData <- sampleData(object)
        colData[[colnamesCol]] <- rownames(colData)
        data <- leftJoin(data, colData, by = colnamesCol)
        data <- encode(data)
        data
    }

args <- c("min", "minMethod", "trans")
formals(`melt,SummarizedExperiment`)[args] <- formals(`melt,matrix`)[args]
rm(args)



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("SummarizedExperiment"),
    definition = `melt,SummarizedExperiment`
)



## Updated 2019-08-26.
`melt,SingleCellExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        counts <- assay(object, i = assay)
        data <- melt(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        colnamesCol <- colnames(data)[[2L]]
        colData <- metrics(object, return = "DataFrame")
        keep <- which(bapply(colData, is.factor))
        colData <- colData[, keep, drop = FALSE]
        colData[[colnamesCol]] <- rownames(colData)
        data <- leftJoin(data, colData, by = colnamesCol)
        data <- encode(data)
        data
    }

formals(`melt,SingleCellExperiment`) <-
    formals(`melt,SummarizedExperiment`)



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("SingleCellExperiment"),
    definition = `melt,SingleCellExperiment`
)
