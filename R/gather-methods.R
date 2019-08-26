## FIXME Deprecating in favor of `gather()`.
## FIXME Need to nuke `reshape2::melt()` dependency.
## FIXME Consider requiring numeric matrix for method.



#' @name gather
#' @inherit bioverbs::gather
#' @note Updated 2019-08-26.
#'
#' @inheritParams acidroxygen::params
#' @param min `integer(1)` or `NULL`.
#'   Minimum count threshold to apply. Filters using "greater than or equal to"
#'   logic internally. Note that this threshold gets applied prior to
#'   logarithmic transformation, when `trans` argument applies.
#' @param minMethod `character(1)`.
#'   Uses [`match.arg()`][base::match.arg].
#'
#'   - `perRow`: *Recommended*. Applies cutoff per row (i.e. gene).
#'     Internally, [`rowSums()`][base::rowSums] values are checked against this
#'     cutoff threshold prior to the melt operation.
#'   - `absolute`: Applies hard cutoff to `counts` column after the melt
#'     operation. This applies to all counts, not per feature.
#' @param trans `character(1)`.
#'   Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#'   to melting, if desired. Use `"identity"` to return unmodified (default).
#' @param ... Additional arguments.
#'
#' @seealso
#' tidyr (recommended):
#'
#' ```r
#' methods("gather")
#' methods("gather_")
#' getS3method("gather", "data.frame", envir = asNamespace("tidyr"))
#' getS3method("gather_", "data.frame", envir = asNamespace("tidyr"))
#' tidyr:::melt_dataframe
#' ```
#'
#' https://github.com/tidyverse/tidyr/blob/master/src/melt.cpp
#' https://github.com/tidyverse/tidyr/blob/master/src/RcppExports.cpp
#'
#' reshape2 (deprecated):
#'
#' ```r
#' help(topic = "melt.array", package = "reshape2")
#' methods("melt")
#' getS3method("melt", "data.array", envir = asNamespace("tidyr"))
#' getS3method("melt", "data.frame", envir = asNamespace("tidyr"))
#' ```
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#' dim(rse)
#' x <- gather(rse, min = NULL)
#' nrow(x)
#' print(x)
NULL



#' @rdname gather
#' @name gather
#' @importFrom bioverbs gather
#' @usage gather(object, ...)
#' @export
NULL



## Updated 2019-08-26.
.gatherMatrix <- function(
    object,
    dimnames = c("rowname", "colname"),
    valueCol = "value"
) {
    assert(
        is.matrix(object),
        hasColnames(object)
    )
    if (is.null(rownames(object))) {
        rownames(object) <- as.character(seq_len(nrow(object)))
    }
    if (is.null(colnames(object))) {
        colnames(object) <- as.character(seq_len(ncol(object)))
    }
    dn <- dimnames(object)
    names(dn) <- dimnames
    labels <- DataFrame(expand.grid(
        dn,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = TRUE
    ))
    value_df <- DataFrame(as.vector(object))
    names(value_df) <- valueCol
    cbind(labels, value_df)
}



## Updated 2019-08-26.
`gather,matrix` <-  # nolint
    function(
        object,
        min = NULL,
        minMethod = c("perRow", "absolute"),
        trans = c("identity", "log2", "log10")
    ) {
        assert(
            hasColnames(object),
            hasRownames(object),
            isInt(min, nullOK = TRUE)
        )
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        if (isInt(min)) {
            assert(
                is.numeric(object),
                isGreaterThanOrEqualTo(min, 1L)
            )
            if (identical(minMethod, "perRow")) {
                rowCutoff <- min
            } else {
                rowCutoff <- 1L
            }
            keep <- rowSums(object) >= rowCutoff
            if (identical(minMethod, "perRow")) {
                message(sprintf(
                    "%d / %d %s passed minimum 'rowSums()' >= %s cutoff.",
                    sum(keep, na.rm = TRUE), nrow(object),
                    ngettext(
                        n = nrow(object),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    rowCutoff
                ))
            }
            object <- object[keep, , drop = FALSE]
            assert(!any(rowSums(object) == 0L))
        }
        valueCol <- "value"
        data <- .gatherMatrix(object, valueCol = valueCol)
        if (isInt(min) && identical(minMethod, "absolute")) {
            nPrefilter <- nrow(data)
            keep <- data[[valueCol]] >= min
            data <- data[keep, , drop = FALSE]
            message(sprintf(
                "%d / %d melted %s passed minimum >= %d expression cutoff.",
                nrow(data),
                nPrefilter,
                ngettext(
                    n = nPrefilter,
                    msg1 = "feature",
                    msg2 = "features"
                ),
                min
            ))
        }
        ## Log transform the value, if desired.
        if (!identical(trans, "identity")) {
            assert(isInt(min))
            message(sprintf("Applying '%s(x + 1L)' transformation.", trans))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert(is.function(fun))
            data[[valueCol]] <- fun(data[[valueCol]] + 1L)
        }
        data <- droplevels(data)
        data
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("matrix"),
    definition = `gather,matrix`
)



## Updated 2019-08-26.
`gather,data.frame` <-  # nolint
    function(object, ...) {
        requireNamespace("tidyr", quietly = FALSE)
        tidyr::gather(object, ...)
    }



#' @describeIn gather Passes to `tidyr::gather()`.
#' @export
setMethod(
    f = "gather",
    signature = signature("data.frame"),
    definition = `gather,data.frame`
)



## Updated 2019-08-26.
`gather,DataFrame` <-  # nolint
    function(
        object,
        keyCol = "key",
        valueCol = "value",
        gatherCols = NULL
    ) {
        assert(
            hasColnames(object),
            isString(keyCol),
            isString(valueCol),
            areDisjointSets(c(keyCol, valueCol), colnames(object))
        )
        if (is.null(gatherCols)) {
            gatherCols <- setdiff(colnames(object), c(keyCol, valueCol))
        }
        assert(
            isSubset(gatherCols, colnames(object)),
            all(bapply(object[, gatherCols], is.atomic)),
            hasLength(
                unlist(unique(lapply(object[, gatherCols], class))),
                n = 1L
            )
        )
        .gatherMatrix(
            object = as.matrix(object[, gatherCols]),
            dimnames = c("rowname", keyCol),
            valueCol = valueCol
        )
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("DataFrame"),
    definition = `gather,DataFrame`
)




## FIXME Matrix method. Use Rle?





## Updated 2019-08-24.
`gather,SummarizedExperiment` <-  # nolint
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
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        ## Get the sample metadata.
        sampleData <- sampleData(object)
        sampleData[["colname"]] <- rownames(sampleData)
        data <- leftJoin(data, sampleData, by = "colname")
        data <- droplevels(data)
        data
    }

args <- c("min", "minMethod", "trans")
formals(`gather,SummarizedExperiment`)[args] <-
    formals(`gather,matrix`)[args]
rm(args)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SummarizedExperiment"),
    definition = `gather,SummarizedExperiment`
)



## Updated 2019-08-24.
`gather,SingleCellExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        ## Note that this will deparse, and can be memory intensive.
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        ## Get the cell-level metrics, which contains sample metadata.
        metrics <- metrics(object, return = "DataFrame")
        keep <- which(bapply(metrics, is.factor))
        metrics <- metrics[, keep, drop = FALSE]
        metrics[["colname"]] <- rownames(metrics)
        ## Join the cell-level metadata.
        data <- leftJoin(data, metrics, by = "colname")
        data <- droplevels(data)
        data <- encode(data)
        data
    }

formals(`gather,SingleCellExperiment`) <-
    formals(`gather,SummarizedExperiment`)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SingleCellExperiment"),
    definition = `gather,SingleCellExperiment`
)
