#' @name gather
#' @inherit bioverbs::gather
#' @note Updated 2019-08-26.
#'
#' @inheritParams acidroxygen::params
#' @param colnames `character(3)`.
#'   Column name mappings for melted data frame return.
#'   Currently only applies to `matrix` and `DataFrame` methods.
#'   Standardized for `SummarizedExperiment` and `SingleCellExperiment`.
#' @param min `integer(1)` or `NULL`.
#'   Minimum count threshold to apply. Filters using "greater than or equal to"
#'   logic internally. Note that this threshold gets applied prior to
#'   logarithmic transformation, when `trans` argument applies.
#' @param minMethod `character(1)`.
#'   Only applies when `min` argument is numeric.
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
.gather <- function(
    object,
    colnames = c("rowname", "colname", "value")
) {
    assert(
        isAny(object, c("matrix", "Matrix")),
        hasDims(object),
        isCharacter(colnames),
        hasLength(colnames, n = 3L),
        areDisjointSets(colnames, colnames(object))
    )
    if (is.null(rownames(object))) {
        rownames(object) <- as.character(seq_len(nrow(object)))
    }
    if (is.null(colnames(object))) {
        colnames(object) <- as.character(seq_len(ncol(object)))
    }
    dn <- dimnames(object)
    names(dn) <- colnames[seq_len(2L)]
    out <- DataFrame(expand.grid(
        dn,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = TRUE
    ))
    value <- DataFrame(as.vector(object))
    names(value) <- colnames[[3L]]
    out <- cbind(out, value)
    out <- encode(out)
    out
}



## Updated 2019-08-26.
`gather,matrix` <-  # nolint
    function(
        object,
        colnames = c("rowname", "colname", "value"),
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
            assert(isGreaterThanOrEqualTo(min, 1L))
            if (identical(minMethod, "perRow")) {
                rowCutoff <- min
            } else {
                rowCutoff <- 1L
            }
            keep <- rowSums(object) >= rowCutoff
            if (identical(minMethod, "perRow")) {
                message(sprintf(
                    "%d / %d %s passed '%s' >= %s cutoff.",
                    sum(keep, na.rm = TRUE),
                    nrow(object),
                    ngettext(
                        n = nrow(object),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    minMethod,
                    rowCutoff
                ))
            }
            object <- object[keep, , drop = FALSE]
            assert(!any(rowSums(object) == 0L))
        }
        valueCol <- colnames[[3L]]
        data <- .gather(object = object, colnames = colnames)
        if (isInt(min) && identical(minMethod, "absolute")) {
            nPrefilter <- nrow(data)
            keep <- data[[valueCol]] >= min
            data <- data[keep, , drop = FALSE]
            message(sprintf(
                "%d / %d %s passed '%s' >= %d expression cutoff.",
                nrow(data),
                nPrefilter,
                ngettext(
                    n = nPrefilter,
                    msg1 = "feature",
                    msg2 = "features"
                ),
                minMethod,
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
        data <- encode(data)
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
`gather,Matrix` <-  # nolint
    appendToBody(
        fun = `gather,matrix`,
        values = quote(rowSums <- Matrix::rowSums)
    )



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("Matrix"),
    definition = `gather,Matrix`
)



## Updated 2019-08-26.
`gather,data.frame` <-  # nolint
    function(object, ...) {
        requireNamespace("tidyr", quietly = FALSE)
        tidyr::gather(object, ...)
    }



#' @describeIn gather Method passes to `tidyr::gather()`.
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
        colnames = c("rowname", "colname", "value")
    ) {
        assert(
            hasColnames(object),
            all(bapply(object, is.atomic)),
            hasLength(unlist(unique(lapply(object, class))), n = 1L)
        )
        .gather(object = as.matrix(object), colnames = colnames)
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("DataFrame"),
    definition = `gather,DataFrame`
)



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
        counts <- assay(object, i = assay)
        data <- gather(
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
formals(`gather,SummarizedExperiment`)[args] <- formals(`gather,matrix`)[args]
rm(args)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SummarizedExperiment"),
    definition = `gather,SummarizedExperiment`
)



## Updated 2019-08-26.
`gather,SingleCellExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        counts <- assay(object, i = assay)
        data <- gather(
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

formals(`gather,SingleCellExperiment`) <-
    formals(`gather,SummarizedExperiment`)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SingleCellExperiment"),
    definition = `gather,SingleCellExperiment`
)
