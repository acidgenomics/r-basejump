#' @name autopadZeros
#' @inherit bioverbs::autopadZeros
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @note For methods on objects supporting [`dim()`][base::dim] (e.g. `matrix`),
#' the object will be returned with the rows and/or columns resorted by default.
#' This does not apply to the `character` method.
#'
#' @return `character`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## character ====
#' autopadZeros(c("A1", "B10"))
#' autopadZeros(c("A1", "B10", "C100"))
#'
#' ## SummarizedExperiment ====
#' rse <- RangedSummarizedExperiment
#' autopadZeros(rse, rownames = TRUE, colnames = TRUE)
#'
#' ## SingleCellExperiment ====
#' sce <- SingleCellExperiment
#' autopadZeros(sce, rownames = TRUE, colnames = TRUE)
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom bioverbs autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`autopadZeros,character` <-  # nolint
    function(object) {
        assert(is.character(object))
        names <- names(object)
        object <- as.character(object)
        assert(validNames(object))
        pattern <- "(.*[A-Za-z])([[:digit:]]+)$"
        ## Early return if no padding is necessary.
        if (!all(grepl(pattern = pattern, x = object))) {
            return(object)
        }
        match <- str_match(string = object, pattern = pattern)
        prefix <- match[, 2L]
        nums <- match[, 3L]
        width <- max(str_length(nums))
        nums <- str_pad(string = nums, width = width, side = "left", pad = "0")
        mat <- matrix(data = c(prefix, nums), ncol = 2L)
        out <- paste0(mat[, 1L], mat[, 2L])
        names(out) <- names
        out
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("character"),
    definition = `autopadZeros,character`
)



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
        if (isTRUE(rownames)) {
            rownames(object) <- autopadZeros(rownames(object))
            if (isTRUE(sort)) {
                object <- object[sort(rownames(object)), , drop = FALSE]
            }
        }
        if (isTRUE(colnames)) {
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



## Updated 2019-07-22.
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
        ## Ensure sample names, which can be defined in `colData()` as
        ## `sampleName` column, also get padded, if necessary. This improves
        ## downstream handling in functions that rely on this feature.
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
