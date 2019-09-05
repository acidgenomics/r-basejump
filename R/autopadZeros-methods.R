#' @name autopadZeros
#' @inherit bioverbs::autopadZeros
#'
#' @note For methods on objects supporting [`dim()`][base::dim] (e.g. `matrix`),
#' the object will be returned with the rows and/or columns resorted by default.
#' This does not apply to the `character` method.
#' @note Updated 2019-09-05.
#'
#' @section SummarizedExperiment sample names:
#'
#' If `sampleName` column is defined in
#' [`colData()`][SummarizedExperiment::colData], these values will also get
#' padded, if necessary. This improves # downstream handling in functions that
#' rely on this feature.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return `character`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## character ====
#' autopadZeros(c("A1", "B10"))
#' autopadZeros(c("A1", "B10", "C100"))
#'
#' ## SummarizedExperiment ====
#' autopadZeros(rse, rownames = TRUE, colnames = TRUE)
#'
#' ## SingleCellExperiment ====
#' autopadZeros(sce, rownames = TRUE, colnames = TRUE)
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom bioverbs autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL



## Updated 2019-09-05.
`autopadZeros,character` <-  # nolint
    function(object) {
        x <- unname(object)
        ## Detect if we need to pad the left or right side automatically.
        leftPattern <- "^([[:digit:]]+)(.+)$"
        rightPattern <- "^(.+)([[:digit:]]+)$"
        if (allAreMatchingRegex(x = x, pattern = leftPattern)) {
            side <- "left"
            pattern <- leftPattern
        } else if (allAreMatchingRegex(x = x, pattern = rightPattern)) {
            side <- "right"
            pattern <- rightPattern
        } else {
            ## Early return if no padding is necessary.
            return(object)
        }
        match <- str_match(string = x, pattern = pattern)
        if (identical(side, "left")) {
            colnames(match) <- c("string", "num", "stem")
        } else if (identical(side, "right")) {
            colnames(match) <- c("string", "stem", "num")
        }
        num <- match[, "num"]
        width <- max(str_length(num))
        num <- str_pad(string = num, width = width, side = "left", pad = "0")
        stem <- match[, "stem"]
        if (identical(side, "left")) {
            x <- paste0(num, stem)
        } else if (identical(side, "right")) {
            x <- paste0(stem, num)
        }
        names(x) <- names(object)
        x
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
