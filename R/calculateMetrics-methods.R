#' @name calculateMetrics
#' @inherit bioverbs::calculateMetrics
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @note Input a raw count matrix. Do not use size factor adjusted or log
#'   normalized counts here.
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param prefilter `logical(1)`.
#'   Drop very low quality samples/cells from the object.
#'   This can resize the number of columns but the rows (i.e. features) do not
#'   change with this operation.
#' @param ... Additional arguments.
#'
#' @return
#' - `matrix` / `Matrix` / `DelayedArray`: `DataFrame` containing metrics.
#' - `SingleCellExperiment` / `SummarizedExperiment`: Modified object, with
#'   metrics in [`colData()`][SummarizedExperiment::colData].
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- calculateMetrics(object)
#' print(x)
NULL



#' @rdname calculateMetrics
#' @name calculateMetrics
#' @importFrom bioverbs calculateMetrics
#' @usage calculateMetrics(object, ...)
#' @export
NULL



## Updated 2019-08-07.
`calculateMetrics,matrix` <-  # nolint
    function(
        object,
        rowRanges = NULL,
        prefilter = FALSE
    ) {
        assert(
            hasValidDimnames(object),
            hasRows(object),
            isAny(rowRanges, c("GRanges", "NULL")),
            isFlag(prefilter)
        )

        message(sprintf(
            fmt = "Calculating %d cellular barcode metrics.",
            ncol(object)
        ))

        codingFeatures <- character()
        mitoFeatures <- character()

        missingBiotype <- function() {
            message(paste0(
                "Calculating metrics without biotype information.\n",
                "`rowRanges` required to calculate: ",
                "nCoding, nMito, mitoRatio"
            ))
        }

        ## Calculate nCoding and nMito, which requires annotations.
        if (!is.null(rowRanges)) {
            assert(
                is(rowRanges, "GRanges"),
                hasValidNames(rowRanges)
            )
            ## Error on missing features.
            setdiff <- setdiff(rownames(object), names(rowRanges))
            if (hasLength(setdiff)) {
                stop(sprintf(
                    fmt = "Features missing in rowRanges: %s",
                    toString(setdiff, width = 200L)
                ))
            }

            ## Subset ranges to match matrix.
            assert(isSubset(rownames(object), names(rowRanges)))
            rowRanges <- rowRanges[rownames(object)]
            rowData <- mcols(rowRanges)
            assert(
                hasRownames(rowData),
                identical(rownames(rowData), names(rowRanges))
            )
            if ("broadClass" %in% colnames(rowData)) {
                ## Drop rows with NA broad class.
                keep <- !is.na(rowData[["broadClass"]])
                assert(is(keep, "Rle"))
                rowData <- rowData[keep, , drop = FALSE]
                ## Coding features.
                keep <- rowData[["broadClass"]] == "coding"
                assert(is(keep, "Rle"))
                codingFeatures <- rownames(rowData[keep, , drop = FALSE])
                message(sprintf(
                    fmt = "%d coding %s detected.",
                    length(codingFeatures),
                    ngettext(
                        n = length(codingFeatures),
                        msg1 = "feature",
                        msg2 = "features"
                    )
                ))
                ## Mitochondrial features.
                keep <- rowData[["broadClass"]] == "mito"
                assert(is(keep, "Rle"))
                mitoFeatures <- rownames(rowData[keep, , drop = FALSE])
                message(sprintf(
                    fmt = "%d mitochondrial %s detected.",
                    length(mitoFeatures),
                    ngettext(
                        n = length(mitoFeatures),
                        msg1 = "feature",
                        msg2 = "features"
                    )
                ))
            } else {
                missingBiotype()
            }
        } else {
            missingBiotype()
        }

        ## Using S4 run-length encoding here to reduce memory overhead.
        ## We're following the naming conventions used in Seurat 3.
        ## Note that "nCount" represents "nUMI" for droplet scRNA-seq data.
        nCount <- Rle(as.integer(colSums(object)))
        nFeature <- Rle(as.integer(colSums(object > 0L)))
        nCoding <- if (hasLength(codingFeatures)) {
            mat <- object[codingFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            Rle(NA_integer_)
        }
        nMito <- if (hasLength(mitoFeatures)) {
            mat <- object[mitoFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            Rle(NA_integer_)
        }
        log10FeaturesPerCount <- log10(nFeature) / log10(nCount)
        mitoRatio <- nMito / nCount
        data <- DataFrame(
            nCount = nCount,
            nFeature = nFeature,
            nCoding = nCoding,
            nMito = nMito,
            log10FeaturesPerCount = log10FeaturesPerCount,
            mitoRatio = mitoRatio,
            row.names = colnames(object)
        )

        ## Apply low stringency cellular barcode pre-filtering.
        ## This keeps only cellular barcodes with non-zero genes.
        if (isTRUE(prefilter)) {
            keep <- !is.na(data[["log10FeaturesPerCount"]])
            assert(is(keep, "Rle"))
            data <- data[keep, , drop = FALSE]

            keep <- data[["nCount"]] > 0L
            assert(is(keep, "Rle"))
            data <- data[keep, , drop = FALSE]

            keep <- data[["nFeature"]] > 0L
            assert(is(keep, "Rle"))
            data <- data[keep, , drop = FALSE]

            message(sprintf(
                fmt = "%d / %d cellular barcodes passed pre-filtering (%s).",
                nrow(data),
                ncol(object),
                percent(nrow(data) / ncol(object))
            ))
        }

        data
    }



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("matrix"),
    definition = `calculateMetrics,matrix`
)



## Updated 2019-08-07.
`calculateMetrics,DelayedArray` <-  # nolint
    appendToBody(
        fun = `calculateMetrics,matrix`,
        values = quote(colSums <- DelayedMatrixStats::colSums2)
    )



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("DelayedArray"),
    definition = `calculateMetrics,DelayedArray`
)



## Updated 2019-08-07.
`calculateMetrics,Matrix` <-  # nolint
    appendToBody(
        fun = `calculateMetrics,matrix`,
        values = quote(colSums <- Matrix::colSums)
    )



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("Matrix"),
    definition = `calculateMetrics,Matrix`
)



## Updated 2019-08-08
.slotMetricsInSE <-
    function(
        object,
        metrics,
        prefilter
    ) {
        assert(
            is(object, "SummarizedExperiment"),
            is(metrics, "DataFrame"),
            isSubset(rownames(metrics), colnames(object)),
            isFlag(prefilter)
        )
        ## Resize the object, if necessary.
        if (isTRUE(prefilter)) {
            object <- object[, rownames(metrics), drop = FALSE]
        }
        ## Update the metrics in column data.
        colData <- colData(object)
        colData <- colData[
            ,
            setdiff(colnames(colData), colnames(metrics)),
            drop = FALSE
            ]
        assert(identical(rownames(colData), rownames(metrics)))
        colData <- cbind(colData, metrics)
        colData(object) <- colData
        validObject(object)
        object
    }



## Updated 2019-08-07.
`calculateMetrics,RangedSummarizedExperiment` <-  # nolint
    function(object) {
        prefilter <- FALSE
        metrics <- calculateMetrics(
            object = counts(object),
            rowRanges = rowRanges(object),
            prefilter = prefilter
        )
        .slotMetricsInSE(
            object = object,
            metrics = metrics,
            prefilter = prefilter
        )
    }



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("RangedSummarizedExperiment"),
    definition = `calculateMetrics,RangedSummarizedExperiment`
)



## Updated 2019-08-08.
`calculateMetrics,SingleCellExperiment` <-  # nolint
    function(object, prefilter = FALSE) {
        counts <- counts(object)
        ## Use DelayedArray for large datasets.
        if (ncol(counts) >= 2E4L) {
            counts <- DelayedArray(counts)
        }
        metrics <- calculateMetrics(
            object = counts,
            rowRanges = rowRanges(object),
            prefilter = prefilter
        )
        .slotMetricsInSE(
            object = object,
            metrics = metrics,
            prefilter = prefilter
        )
    }



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("SingleCellExperiment"),
    definition = `calculateMetrics,SingleCellExperiment`
)
