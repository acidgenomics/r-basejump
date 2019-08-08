## FIXME nGene to nFeature
## FIXME log10GenesPerUMI to log10FeaturesPerUMI
## FIXME Rework to not use dplyr / tibble here.
## FIXME Look for nUMI and use that first, otherwise pick nCount
## FIXME Look for nGene and use that, otherwise pick nFeature
## FIXME Look for log10GenesPerUMI, log10FeaturesPerUMI


#' @name filterCells
#' @author Michael Steinbaugh
#' @inherit bioverbs::filterCells
#' @note Updated 2019-08-08.
#'
#' @details
#' Apply feature (i.e. gene/transcript) detection, novelty score, and
#' mitochondrial abundance cutoffs to cellular barcodes. By default we recommend
#' applying the same filtering cutoff to all samples. The filtering parameters
#' now support per-sample cutoffs, defined using a named `numeric` vector. When
#' matching per sample, be sure to use the [sampleNames()] return values (i.e.
#' the `sampleName` column in [sampleData()]).
#'
#' @inheritParams acidroxygen::params
#' @param nCells `integer(1)`.
#'   Expected number of cells per sample.
#'   Don't set this by default, unless you're confident of your capture.
#' @param minCounts,maxCounts `integer(1)`.
#'   Minimum/maximum number of counts per cell.
#'   Applies to UMI disambiguated counts for droplet scRNA-seq.
#'   Matches `nUMI` then `nCount` column in
#'   [`colData()`][SummarizedExperiment] internally.
#'   Previously named `minUMIs`/`maxUMIs` in bcbioSingleCell.
#' @param minFeatures,maxFeatures `integer(1)`.
#'   Minimum/maximum number of features (i.e. genes) detected.
#'   Matches `nFeature`in [`colData()`][SummarizedExperiment] internally.
#'   Previously named `minGenes`/`maxGenes` in bcbioSingleCell.
#' @param minNovelty `integer(1)` (`0`-`1`).
#'   Minimum novelty score (log10 features per UMI).
#'   Matches `log10FeaturesPerCount` then `log10FeaturesPerUMI` (legacy)
#'   [`colData()`][SummarizedExperiment] internally.
#' @param maxMitoRatio `integer(1)` (`0`-`1`).
#'   Maximum relative mitochondrial abundance.
#' @param minCellsPerFeature `integer(1)`.
#'   Include genes with non-zero expression in at least this many cells.
#'   Previously named `minCellsPerGene` in bcbioSingleCell.
#' @param ... Additional arguments.
#'
#' @return `SingleCellExperiment`.
#' Filtering information gets slotted into [`metadata()`][S4Vectors::metadata]
#' as `filterCells` metadata.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' show(object)
#'
#' x <- filterCells(object)
#' show(x)
#'
#' ## Per sample cutoffs.
#' sampleNames(object)
#' x <- filterCells(
#'     object = object,
#'     minCounts = c(sample1 = 100L)
#' )
#' show(x)
NULL



#' @rdname filterCells
#' @name filterCells
#' @importFrom bioverbs filterCells
#' @usage filterCells(object, ...)
#' @export
NULL



## Updated 2019-08-08.
.isFiltered <- function(object) {
    if (!is.null(metadata(object)[["filterCells"]])) {
        TRUE
    } else {
        FALSE
    }
}



## Updated 2019-07-24.
.paddedCount <- function(x, width = 8L) {
    str_pad(x, width = width, pad = " ")
}



## FIXME Move this to goalie package.
## Updated 2019-08-08.
.hasMetrics <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    ok <- isSubset(
        x = c("nCount", "nFeature"),
        y = colnames(colData(object))
    )
    if (!isTRUE(ok)) {
        return(false("`calculateMetrics()` needs to be run on this object."))
    }
    TRUE
}



#' @param metrics `DataFrame`.
#'   Quality control metrics.
#' @param param
#'   Name of parameter to evaluate.
#'   Supports multiple values as a named vector.
#' @param params,sampleNames Scoped from parent.
#' @param op `character(1)`.
#'   Relational operator.
.filterCellsByMetric <- function(
    metrics,
    param,
    colname,
    op = c(
        gte = ">=",
        lte = "<=",
        gt = ">",
        lt = "<",
        eq = "=="
    ),
    params = get(
        x = "params",
        envir = parent.frame(),
        inherits = FALSE
    ),
    sampleNames = get(
        x = "sampleNames",
        envir = parent.frame(),
        inherits = FALSE
    )
) {
    assert(
        is(metrics, "DataFrame"),
        hasRownames(metrics),
        isString(param),
        is.list(params),
        isString(colname),
        isSubset(colname, colnames(metrics)),
        isCharacter(sampleNames)
    )
    arg <- params[[param]]
    assert(is.numeric(arg))
    op <- unname(match.arg(op))

    if (length(arg) > 1L) {
        assert(identical(names(arg), sampleNames))
        message(sprintf(
            fmt = "%s per sample mode\n%s",
            param,
            printString(arg)
        ))
        ## Split metrics into DataFrameList by sampleID.
        split <- split(
            x = metrics,
            f = metrics[["sampleID"]]
        )
        assert(is(split, "SplitDataFrameList"))
        ## Now loop across the list and apply our filtering criteria.
        xxx <- mapply(
            data = split,
            cutoff = arg,
            FUN = function(data, cutoff, op) {
                op <- get(op, inherits = TRUE)
                assert(
                    is.function(op),
                    is.primitive(op)
                )
                keep <- do.call(
                    what = op,
                    args = list(
                        e1,
                        e2
                    )
                )

            },
            MoreArgs = list(op = op),
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )



        split[[colname]]

        list <- mapply(
            sample = names(arg),
            cutoff = arg,
            FUN = function(sample, cutoff, metrics, colname) {
                ## Select sample.
                keep <- metrics[["sampleName"]] == sample
                metrics <- metrics[keep, , drop = FALSE]

                keep <- metrics[[colname]]
                metrics <- metrics[keep, , drop = FALSE]
                metrics
            },
            MoreArgs = list(
                metrics = metrics,
                colname = colname
            ),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        metrics <- unlist(DataFrameList(list), use.names = TRUE)
        assert(
            is(metrics, "DataFrame"),
            hasRownames(metrics)
        )
    } else {
        ## FIXME Use base R instead.
        colData <- filter(colData, !!sym("nCount") >= !!minCounts)
    }
    if (!nrow(colData)) {
        stop("No cells passed `minCounts` cutoff")
    }

    summaryCells[["minCounts"]] <- paste(
        paste(.paddedCount(nrow(colData)), "cells"),
        paste("minCounts", ">=", min(minCounts)),
        sep = " | "
    )
}


## FIXME Add .filterFeatures, with margin?



## Updated 2019-08-08.
`filterCells,SingleCellExperiment` <-  # nolint
    function(
        object,
        nCells = Inf,
        minCounts = 1L,
        maxCounts = Inf,
        minFeatures = 1L,
        maxFeatures = Inf,
        minNovelty = 0L,
        maxMitoRatio = 1L,
        minCellsPerFeature = 1L
    ) {
        validObject(object)
        assert(
            ## Check to see if `calculateMetrics()` needs to be run.
            .hasMetrics(object),
            ## nCells
            all(isIntegerish(nCells)),
            all(isPositive(nCells)),
            ## minCounts
            all(isIntegerish(minCounts)),
            all(isPositive(minCounts)),
            ## maxCounts
            all(isIntegerish(maxCounts)),
            all(isPositive(maxCounts)),
            ## minFeatures
            all(isIntegerish(minFeatures)),
            all(isPositive(minFeatures)),
            ## maxFeatures
            all(isIntegerish(maxFeatures)),
            all(isNonNegative(maxFeatures)),
            ## minNovelty
            all(isInRange(minNovelty, lower = 0L, upper = 1L)),
            ## maxMitoRatio
            all(isInLeftOpenRange(maxMitoRatio, lower = 0L, upper = 1L)),
            ## minCellsPerFeature
            all(isIntegerish(minCellsPerFeature)),
            all(isPositive(minCellsPerFeature))
        )

        ## FIXME Split into cell (column) and feature (row) params.
        params <- list(
            nCells = nCells,
            minCounts = minCounts,
            maxCounts = maxCounts,
            minFeatures = minFeatures,
            maxFeatures = maxFeatures,
            minNovelty = minNovelty,
            maxMitoRatio = maxMitoRatio,
            minCellsPerFeature = minCellsPerFeature
        )

        originalDim <- dim(object)
        sampleNames <- sampleNames(object)
        ## Using DataFrame with Rle instead of tibble for improved speed.
        metrics <- colData(object)

        ## Filter low quality cells --------------------------------------------
        message(sprintf("Filtering %d cells.", ncol(object)))

        # FIXME Loop across the params to do cell filtering.

        ## minCounts
        .filterCellsByMetric(
            metrics = metrics,
            param = "minCounts"
        )


        ## maxCounts
        if (!is.null(names(maxCounts))) {
            assert(areSetEqual(names(maxCounts), sampleNames))
            message(paste(
                "maxCounts: per sample mode",
                printString(maxCounts),
                sep = "\n"
            ))
            list <- mapply(
                sample = names(maxCounts),
                cutoff = maxCounts,
                FUN = function(sample, cutoff) {
                    ## FIXME Use base R instead.
                    filter(
                        colData,
                        !!sym("sampleName") == !!sample,
                        !!sym("nCount") <= !!cutoff
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
            ## FIXME Use base R instead.
            colData <- bind_rows(list)
        } else {
            ## FIXME Use base R instead.
            colData <- filter(colData, !!sym("nCount") <= !!maxCounts)
        }
        if (!nrow(colData)) {
            stop("No cells passed `maxCounts` cutoff")
        }
        summaryCells[["maxCounts"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("maxCounts", "<=", max(maxCounts)),
            sep = " | "
        )

        ## minFeatures
        if (!is.null(names(minFeatures))) {
            assert(areSetEqual(names(minFeatures), sampleNames))
            message(paste(
                "minFeatures: per sample mode",
                printString(minFeatures),
                sep = "\n"
            ))
            list <- mapply(
                sample = names(minFeatures),
                cutoff = minFeatures,
                FUN = function(sample, cutoff) {
                    filter(
                        colData,
                        !!sym("sampleName") == !!sample,
                        !!sym("nFeature") >= !!cutoff
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
            colData <- bind_rows(list)
        } else {
            colData <- filter(colData, !!sym("nFeature") >= !!minFeatures)
        }
        if (!nrow(colData)) {
            stop("No cells passed `minFeatures` cutoff")
        }
        summaryCells[["minFeatures"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("minFeatures", ">=", min(minFeatures)),
            sep = " | "
        )

        ## maxFeatures
        if (!is.null(names(maxFeatures))) {
            assert(areSetEqual(names(maxFeatures), sampleNames))
            message(paste(
                "maxFeatures: per sample mode",
                printString(maxFeatures),
                sep = "\n"
            ))
            list <- mapply(
                sample = names(maxFeatures),
                cutoff = maxFeatures,
                FUN = function(sample, cutoff) {
                    filter(
                        colData,
                        !!sym("sampleName") == !!sample,
                        !!sym("nFeature") <= !!cutoff
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
            colData <- bind_rows(list)
        } else {
            colData <- filter(colData, !!sym("nFeature") <= !!maxFeatures)
        }
        if (!nrow(colData)) {
            stop("No cells passed `maxFeatures` cutoff")
        }
        summaryCells[["maxFeatures"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("maxFeatures", "<=", max(maxFeatures)),
            sep = " | "
        )

        ## minNovelty
        if (!is.null(names(minNovelty))) {
            assert(areSetEqual(names(minNovelty), sampleNames))
            message(paste(
                "minNovelty: per sample mode",
                printString(minNovelty),
                sep = "\n"
            ))
            list <- mapply(
                sample = names(minNovelty),
                cutoff = minNovelty,
                FUN = function(sample, cutoff) {
                    filter(
                        colData,
                        !!sym("sampleName") == !!sample,
                        !!sym("log10FeaturesPerCount") >= !!cutoff)
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
            colData <- bind_rows(list)
        } else {
            colData <- filter(
                colData,
                !!sym("log10FeaturesPerCount") >= !!minNovelty
            )
        }
        if (!nrow(colData)) {
            stop("No cells passed `minNovelty` cutoff")
        }
        summaryCells[["minNovelty"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("minNovelty", "<=", min(minNovelty)),
            sep = " | "
        )

        ## maxMitoRatio
        if (!is.null(names(maxMitoRatio))) {
            assert(areSetEqual(names(maxMitoRatio), sampleNames))
            message(paste(
                "maxMitoRatio: per sample mode",
                printString(maxMitoRatio),
                sep = "\n"
            ))
            list <- mapply(
                sample = names(maxMitoRatio),
                cutoff = maxMitoRatio,
                FUN = function(sample, cutoff) {
                    filter(
                        colData,
                        !!sym("sampleName") == !!sample,
                        !!sym("mitoRatio") <= !!cutoff
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
            colData <- bind_rows(list)
        } else {
            colData <- filter(colData, !!sym("mitoRatio") <= !!maxMitoRatio)
        }
        if (!nrow(colData)) {
            stop("No cells passed `maxMitoRatio` cutoff")
        }
        summaryCells[["maxMitoRatio"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("maxMitoRatio", "<=", max(maxMitoRatio)),
            sep = " | "
        )

        ## Expected number of cells per sample.
        ## Filtering here by top counts.
        if (nCells < Inf) {
            colData <- colData %>%
                group_by(!!sym("sampleID")) %>%
                arrange(desc(!!sym("nUMI")), .by_group = TRUE) %>%
                slice(seq_len(nCells))
        }

        if (!nrow(colData)) {
            stop("No cells passed `nCells` cutoff")
        }
        summaryCells[["nCells"]] <- paste(
            paste(.paddedCount(nrow(colData)), "cells"),
            paste("nCells", "==", nCells),
            sep = " | "
        )

        ## Now coerce back to DataFrame from tibble.
        colData <- as(colData, "DataFrame")
        assert(hasRownames(colData))
        cells <- sort(rownames(colData))
        assert(isSubset(cells, colnames(object)))
        object <- object[, cells, drop = FALSE]

        ## Filter low quality features (i.e. genes) ----------------------------
        summaryFeatures <- character()
        summaryFeatures[["prefilter"]] <- paste(
            paste(.paddedCount(nrow(object)), "features"),
            "prefilter",
            sep = " | "
        )
        if (minCellsPerFeature > 0L) {
            nonzero <- counts(object) > 0L
            keep <- rowSums(nonzero) >= minCellsPerFeature
            features <- names(keep)[keep]
        } else {
            features <- rownames(object)
        }
        if (!length(features)) {
            stop("No features passed `minCellsPerFeature` cutoff")
        }
        summaryFeatures[["minCellsPerFeature"]] <- paste(
            paste(.paddedCount(length(features)), "features"),
            paste("minCellsPerFeature", ">=", as.character(minCellsPerFeature)),
            sep = " | "
        )
        features <- sort(features)
        assert(isSubset(features, rownames(object)))
        object <- object[features, , drop = FALSE]

        ## Summary -------------------------------------------------------------
        summaryParams <- paste("  -", c(
            paste(">=", min(minCounts), "UMIs per cell"),
            paste("<=", max(maxCounts), "UMIs per cell"),
            paste(">=", min(minFeatures), "features per cell"),
            paste("<=", max(maxFeatures), "features per cell"),
            paste(">=", min(minNovelty), "novelty score"),
            paste("<=", max(maxMitoRatio), "mitochondrial abundance"),
            paste("==", nCells, "cells per sample"),
            paste(">=", min(minCellsPerFeature), "cells per feature")
        ))
        summary <- c(
            "Parameters:",
            summaryParams,
            separatorBar,
            "Cells:",
            as.character(summaryCells),
            paste(
                .paddedCount(dim(object)[[2L]]), "of", originalDim[[2L]],
                "cells passed filtering",
                paste0(
                    "(", percent(dim(object)[[2L]] / originalDim[[2L]]), ")"
                )
            ),
            ## Number of cells per sample.
            printString(table(colData[["sampleName"]])),
            separatorBar,
            "Features:",
            as.character(summaryFeatures),
            paste(
                .paddedCount(dim(object)[[1L]]), "of", originalDim[[1L]],
                "features passed filtering",
                paste0(
                    "(", percent(dim(object)[[1L]] / originalDim[[1L]]), ")"
                )
            )
        )
        message(paste(summary, collapse = "\n"))

        ## Metadata ------------------------------------------------------------
        ## FIXME Move this to bcbioSingleCell.
        ## if (isSubset(
        ##     x = "cellularBarcodes",
        ##     y = names(metadata(object))
        ## )) {
        ##     metadata(object)[["cellularBarcodes"]] <- NULL
        ## }

        metadata(object)[["filterCells"]] <- list(
            cells = cells,
            features = features,
            params = params,
            ## FIXME summary = summary,
            call = match.call()
        )

        object
    }



#' @rdname filterCells
#' @export
setMethod(
    f = "filterCells",
    signature = signature("SingleCellExperiment"),
    definition = `filterCells,SingleCellExperiment`
)
