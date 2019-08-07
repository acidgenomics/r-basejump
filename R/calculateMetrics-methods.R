#' Calculate cellular barcode quality metrics
#'
#' @name calculateMetrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Input a raw count matrix. Do not use size factor adjusted or log
#'   normalized counts here.
#' @note Updated 2019-08-07.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#' counts <- counts(indrops)
#' rowRanges <- rowRanges(indrops)
#'
#' # Metrics using genome annotations (recommended).
#' x <- calculateMetrics(counts, rowRanges = rowRanges)
#' head(x)
#'
#' # Minimal metrics (supported, but not recommended).
#' x <- calculateMetrics(counts, rowRanges = NULL)
#' head(x)
NULL



## Updated 2019-08-07.
`calculateMetrics,DelayedArray` <-  # nolint
    function(
        object,
        rowRanges = NULL,
        prefilter = FALSE
    ) {
        ## This provides easy method support for `sparseMatrix` class.
        if (!is(object, "DelayedArray")) {
            object <- DelayedArray(object)
        }
        ## Ensure we're calling from DelayedMatrixStats, and not matrixStats
        ## or Matrix packages.
        colSums <- DelayedMatrixStats::colSums2

        ## Using shared method code for dense and sparseMatrix.
        assert(
            hasValidDimnames(object),
            hasRows(object),
            isAny(rowRanges, c("GRanges", "NULL")),
            isFlag(prefilter)
        )

        message(sprintf(
            fmt = "Calculating %d cellular barcode metrics.",
            ncol(counts)
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

        ## Using DelayedMatrixStats approach here (see above).
        ## Using S4 run-length encoding here to reduce memory overhead.
        ## Consider renaming "UMI" to something more general here.
        nUMI <- Rle(as.integer(colSums(object)))
        nGene <- Rle(as.integer(colSums(object > 0L)))
        nCoding <- if (hasLength(codingFeatures)) {
            mat <- object[codingFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            NA_integer_
        }
        nMito <- if (hasLength(mitoFeatures)) {
            mat <- object[mitoFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            NA_integer_
        }
        log10GenesPerUMI <- log10(nGene) / log10(nUMI)
        mitoRatio <- nMito / nUMI
        data <- DataFrame(
            nUMI = nUMI,
            nGene = nGene,
            nCoding = nCoding,
            nMito = nMito,
            log10GenesPerUMI = log10GenesPerUMI,
            mitoRatio = mitoRatio,
            row.names = colnames(object)
        )

        ## Apply low stringency cellular barcode pre-filtering.
        ## This keeps only cellular barcodes with non-zero genes.
        if (isTRUE(prefilter)) {
            keep <- !is.na(data[["log10GenesPerUMI"]])
            assert(is(keep, "Rle"))
            data <- data[keep, , drop = FALSE]

            keep <- data[["nUMI"]] > 0L
            assert(is(keep, "Rle"))
            data <- data[keep, , drop = FALSE]

            keep <- data[["nGene"]] > 0L
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
    signature = signature("DelayedArray"),
    definition = `calculateMetrics,DelayedArray`
)



## Updated 2019-08-07.
`calculateMetrics,Matrix` <-  # nolint
    `calculateMetrics,DelayedArray`



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature("Matrix"),
    definition = `calculateMetrics,Matrix`
)



## Updated 2019-08-07.
`calculateMetrics,SingleCellExperiment` <-  # nolint
    function(object, prefilter = FALSE) {
        calculateMetrics(
            object = counts(object),
            rowRanges = rowRanges(object),
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
