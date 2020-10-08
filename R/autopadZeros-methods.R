#' @name autopadZeros
#' @inherit AcidGenerics::autopadZeros
#'
#' @note For methods on objects supporting [`dim()`][base::dim] (e.g. `matrix`),
#' the object will be returned with the rows and/or columns resorted by default.
#' This does not apply to the `character` method defined in syntactic.
#' @note Updated 2020-06-15.
#'
#' @section SummarizedExperiment sample names:
#'
#' If `sampleName` column is defined in
#' [`colData()`][SummarizedExperiment::colData], these values will also get
#' padded, if necessary. This improves # downstream handling in functions that
#' rely on this feature.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `character`.
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
#' autopadZeros(object, rownames = TRUE, colnames = TRUE)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' autopadZeros(object, rownames = TRUE, colnames = TRUE)
NULL



## Updated 2019-07-22.
`autopadZeros,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        sort = TRUE
    ) {
        assert(
            hasValidDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(sort)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- autopadZeros(rownames(object))
            if (isTRUE(sort)) {
                object <- object[sort(rownames(object)), , drop = FALSE]
            }
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- autopadZeros(colnames(object))
            if (isTRUE(sort)) {
                object <- object[, sort(colnames(object)), drop = FALSE]
            }
        }
        object
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("matrix"),
    definition = `autopadZeros,matrix`
)



## Updated 2019-08-05.
`autopadZeros,SummarizedExperiment` <-  # nolint
    function(object, rownames = FALSE, colnames = TRUE, sort = TRUE) {
        object <- do.call(
            what = `autopadZeros,matrix`,
            args = list(
                object = object,
                rownames = rownames,
                colnames = colnames,
                sort = sort
            )
        )
        if ("sampleName" %in% colnames(colData(object))) {
            sampleNames(object) <- autopadZeros(sampleNames(object))
        }
        object
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("SummarizedExperiment"),
    definition = `autopadZeros,SummarizedExperiment`
)
