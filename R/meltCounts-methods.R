#' @name meltCounts
#' @inherit bioverbs::meltCounts
#' @note Updated 2019-08-19.
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
#' x <- meltCounts(rse, minCounts = NULL)
#' nrow(x)
#' print(x)
NULL



#' @rdname meltCounts
#' @name meltCounts
#' @importFrom bioverbs meltCounts
#' @usage meltCounts(object, ...)
#' @export
NULL



## Updated 2019-08-19.
`meltCounts,matrix` <-  # nolint
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
            if (minCountsMethod == "perFeature") {
                rowCutoff <- minCounts
            } else {
                rowCutoff <- 1L
            }
            keep <- rowSums(object) >= rowCutoff
            if (minCountsMethod == "perFeature") {
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
        data <- melt(
            data = object,
            varnames = c("rowname", "colname"),
            value.name = "counts"
        )
        data <- as(data, "DataFrame")
        ## When applying an absolute threshold using `minCountsMethod`, apply
        ## this cutoff prior to logarithmic transformation.
        if (isInt(minCounts) && minCountsMethod == "absolute") {
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
        if (trans != "identity") {
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



#' @rdname meltCounts
#' @export
setMethod(
    f = "meltCounts",
    signature = signature("matrix"),
    definition = `meltCounts,matrix`
)



## Updated 2019-08-06.
`meltCounts,SummarizedExperiment` <-  # nolint
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
        ## Get the sample metadata.
        sampleData <- sampleData(object)
        sampleData[["colname"]] <- rownames(sampleData)
        ## Passing to matrix method.
        data <- meltCounts(
            object = counts,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        data <- left_join(data, sampleData, by = "colname")
        data <- droplevels(data)
        data
    }

args <- c("minCounts", "minCountsMethod", "trans")
formals(`meltCounts,SummarizedExperiment`)[args] <-
    formals(`meltCounts,matrix`)[args]
rm(args)



#' @rdname meltCounts
#' @export
setMethod(
    f = "meltCounts",
    signature = signature("SummarizedExperiment"),
    definition = `meltCounts,SummarizedExperiment`
)
