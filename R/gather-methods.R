## FIXME Deprecating in favor of `gather()`.
## FIXME Need to nuke `reshape2::melt()` dependency.



#' @name gather
#' @inherit bioverbs::gather
#' @note Updated 2019-08-24.
#'
#' @inheritParams acidroxygen::params
#' @param minCounts `integer(1)` or `NULL`.
#'   Minimum count threshold to apply. Disable with `NULL`. Filters using
#'   "greater than or equal to" logic internally. Note that this threshold gets
#'   applied prior to logarithmic transformation, when `trans` argument applies.
#' @param minCountsMethod `character(1)`.
#'   Uses [`match.arg()`][base::match.arg].
#'
#'   - `perFeature`: *Recommended*. Applies cutoff per row feature (i.e. gene).
#'     Internally, [`rowSums()`][base::rowSums] values are checked against this
#'     cutoff threshold prior to the melt operation.
#'   - `absolute`: Applies hard cutoff to `counts` column after the melt
#'     operation. This applies to all counts, not per feature.
#' @param trans `character(1)`.
#'   Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#'   to melting, if desired. Use `"identity"` to return unmodified (default).
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#' dim(rse)
#' x <- gather(rse, minCounts = NULL)
#' nrow(x)
#' print(x)
NULL



#' @rdname gather
#' @name gather
#' @importFrom bioverbs gather
#' @usage gather(object, ...)
#' @export
NULL



## Updated 2019-08-24.
`gather,matrix` <-  # nolint
    function(
        object,
        minCounts = 1L,
        minCountsMethod = c("perFeature", "absolute"),
        trans = c("identity", "log2", "log10")
    ) {
        validObject(object)
        assert(isInt(minCounts, nullOK = TRUE))
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Filter rows that don't pass our `minCounts` expression cutoff. Note
        ## that we're ensuring rows containing all zeros are always dropped,
        ## even when `minCountsMethod = "absolute"`.
        if (isInt(minCounts)) {
            assert(isGreaterThanOrEqualTo(minCounts, 1L))
            if (identical(minCountsMethod, "perFeature")) {
                rowCutoff <- minCounts
            } else {
                rowCutoff <- 1L
            }
            keep <- rowSums(object) >= rowCutoff
            if (identical(minCountsMethod, "perFeature")) {
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
            ## Ensure that no zero rows propagate.
            assert(!any(rowSums(object) == 0L))
        }
        ## Using reshape2 array (matrix) method here.
        ## > help(topic = "melt.array", package = "reshape2")
        ## nolint end
        ## FIXME Remove, this isn't maintained anymore.s
        data <- melt(
            data = object,
            varnames = c("rowname", "colname"),
            value.name = "counts"
        )
        data <- as(data, "DataFrame")
        ## When applying an absolute threshold using `minCountsMethod`, apply
        ## this cutoff prior to logarithmic transformation.
        if (
            isInt(minCounts) &&
            identical(minCountsMethod, "absolute")
        ) {
            nPrefilter <- nrow(data)
            keep <- data[["counts"]] >= minCounts
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
                minCounts
            ))
        }
        ## Log transform the counts, if desired.
        if (!identical(trans, "identity")) {
            assert(isInt(minCounts))
            message(sprintf("Applying '%s(x + 1L)' transformation.", trans))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert(is.function(fun))
            data[["counts"]] <- fun(data[["counts"]] + 1L)
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



## Updated 2019-08-24.
`gather,data.frame` <-  # nolint
    function(object, ...) {
        assert(requireNamespace("tidyr", quietly = TRUE))
        tidyr::gather(data = object, ...)
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("data.frame"),
    definition = `gather,data.frame`
)



## FIXME Matrix method. Use Rle?
## FIXME DataFrame method. Use Rle?.



## Updated 2019-08-24.
`gather,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        minCounts,
        minCountsMethod,
        trans
    ) {
        validObject(object)
        assert(isScalar(assay))
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        ## Get the sample metadata.
        sampleData <- sampleData(object)
        sampleData[["colname"]] <- rownames(sampleData)
        data <- left_join(data, sampleData, by = "colname")
        data <- droplevels(data)
        data
    }

args <- c("minCounts", "minCountsMethod", "trans")
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
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        ## Note that this will deparse, and can be memory intensive.
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        ## Get the cell-level metrics, which contains sample metadata.
        metrics <- metrics(object, return = "DataFrame")
        keep <- which(bapply(metrics, is.factor))
        metrics <- metrics[, keep, drop = FALSE]
        metrics[["colname"]] <- rownames(metrics)
        ## Join the cell-level metadata.
        data <- left_join(data, metrics, by = "colname")
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
