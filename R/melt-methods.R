#' @name melt
#' @inherit acidgenerics::melt
#' @note Updated 2020-01-20.
#'
#' @inheritParams acidroxygen::params
#' @param colnames `character(3)`.
#'   Column name mappings for melted data frame return.
#'   Currently only applies to `matrix` and `DataFrame` methods.
#'   Standardized for `SummarizedExperiment` and `SingleCellExperiment`.
#' @param min `numeric(1)` or `NULL`.
#'   Minimum count threshold to apply. Filters using "greater than or equal to"
#'   logic internally. Note that this threshold gets applied prior to
#'   logarithmic transformation, when `trans` argument applies.
#'   Use `-Inf` or `NULL` to disable.
#' @param minMethod `character(1)`.
#'   Only applies when `min` argument is numeric.
#'   Uses [`match.arg()`][base::match.arg].
#'
#'   - `absolute`: Applies hard cutoff to `counts` column after the melt
#'     operation. This applies to all counts, not per feature.
#'   - `perRow`: Applies cutoff per row (i.e. gene). Internally,
#'     [`rowSums()`][base::rowSums] values are checked against this cutoff
#'     threshold prior to the melt operation.
#' @param trans `character(1)`.
#'   Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#'   to melting, if desired. Use `"identity"` to return unmodified (default).
#' @param ... Additional arguments.
#'
#' @seealso
#' tidyr:
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
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
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



#' @rdname melt
#' @name melt
#' @importFrom acidgenerics melt
#' @usage melt(object, ...)
#' @export
NULL



## Updated 2019-09-01.
.melt <- function(
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
    out
}



## Updated 2020-01-20.
`melt,matrix` <-  # nolint
    function(
        object,
        colnames = c("rowname", "colname", "value"),
        min = -Inf,
        minMethod = c("absolute", "perRow"),
        trans = c("identity", "log2", "log10")
    ) {
        assert(
            hasColnames(object),
            hasRownames(object),
            isNumber(min, nullOK = TRUE)
        )
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        if (
            identical(minMethod, "perRow") &&
            isTRUE(is.finite(min))
        ) {
            keep <- rowSums(object) >= min
            if (identical(minMethod, "perRow")) {
                cli_alert_info(sprintf(
                    "%d / %d %s passed {.arg %s} >= {.val %s} cutoff.",
                    sum(keep, na.rm = TRUE),
                    nrow(object),
                    ngettext(
                        n = nrow(object),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    minMethod,
                    as.character(min)
                ))
            }
            object <- object[keep, , drop = FALSE]
        }
        valueCol <- colnames[[3L]]
        data <- .melt(object = object, colnames = colnames)
        data <- encode(data)
        if (
            identical(minMethod, "absolute") &&
            isTRUE(is.finite(min))
        ) {
            nPrefilter <- nrow(data)
            keep <- data[[valueCol]] >= min
            data <- data[keep, , drop = FALSE]
            cli_alert_info(sprintf(
                "%d / %d %s passed {.arg %s} >= {.val %s} expression cutoff.",
                nrow(data),
                nPrefilter,
                ngettext(
                    n = nPrefilter,
                    msg1 = "feature",
                    msg2 = "features"
                ),
                minMethod,
                as.character(min)
            ))
        }
        ## Log transform the value, if desired.
        if (!identical(trans, "identity")) {
            assert(isInt(min))
            cli_alert(sprintf(
                "Applying {.fun %s(x + 1L)} transformation.", trans
            ))
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



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("matrix"),
    definition = `melt,matrix`
)



## Updated 2019-09-01.
`melt,table` <- .melt  # nolint



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("table"),
    definition = `melt,table`
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



## Updated 2019-09-01.
`melt,DataFrame` <-  # nolint
    function(
        object,
        colnames = c("rowname", "colname", "value")
    ) {
        assert(
            hasColnames(object),
            all(bapply(object, is.atomic)),
            hasLength(unlist(unique(lapply(object, class))), n = 1L)
        )
        melt(object = as.matrix(object), colnames = colnames)
    }



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature("DataFrame"),
    definition = `melt,DataFrame`
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
