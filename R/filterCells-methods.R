## FIXME MOVE THIS TO ACIDSINGLECELL PACKAGE?



#' @name filterCells
#' @inherit AcidGenerics::filterCells
#' @note Updated 2021-02-02.
#'
#' @details
#' Apply feature (i.e. gene/transcript) detection, novelty score, and
#' mitochondrial abundance cutoffs to cellular barcodes. By default we recommend
#' applying the same filtering cutoff to all samples. The filtering parameters
#' now support per-sample cutoffs, defined using a named `numeric` vector. When
#' matching per sample, be sure to use the [sampleNames()] return values (i.e.
#' the `sampleName` column in [sampleData()]).
#'
#' Filtering information gets slotted into [`metadata()`][S4Vectors::metadata]
#' as `filterCells` metadata.
#'
#' @inheritParams AcidRoxygen::params
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
#' @param countsCol,featuresCol,noveltyCol,mitoRatioCol `character(1)`.
#'   Column mapping name.
#' @param ... Additional arguments.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- filterCells(object)
#' print(x)
#'
#' ## Per sample cutoffs.
#' sampleNames(object)
#' x <- filterCells(
#'     object = object,
#'     minCounts = c(sample1 = 100L)
#' )
#' print(x)
NULL



## Updated 2021-02-22.
`filterCells,SingleCellExperiment` <-  # nolint
    function(
        object,
        ## Cell-level metrics.
        minCounts = 1L,
        maxCounts = Inf,
        minFeatures = 1L,
        maxFeatures = Inf,
        minNovelty = 0L,
        maxMitoRatio = 1L,
        ## Feature-level metrics.
        minCellsPerFeature = 1L,
        ## Manually subset top cells by sequencing depth.
        nCells = Inf,
        ## Column mappings.
        countsCol = "nCount",
        featuresCol = "nFeature",
        noveltyCol = "log10FeaturesPerCount",
        mitoRatioCol = "mitoRatio"
    ) {
        validObject(object)
        assert(
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
            all(isNonNegative(minCellsPerFeature))
        )
        cli_alert("Filtering cells with {.fun filterCells}.")
        ## Calculate metrics, if necessary.
        if (!hasMetrics(object, colData = c("nCount", "nFeature"))) {
            object <- calculateMetrics(object)
        }
        ## Using DataFrame with Rle instead of tibble for improved speed.
        metrics <- colData(object)
        colnames(metrics) <- camelCase(colnames(metrics), strict = TRUE)
        sampleIds <- names(sampleNames(object))
        assert(isSubset(sampleIds, levels(metrics[["sampleId"]])))
        originalDim <- dim(object)
        ## Detect low quality cells --------------------------------------------
        ## Check that the requested column names for filtering match.
        assert(isSubset(
            x = c(countsCol, featuresCol, noveltyCol, mitoRatioCol),
            y = colnames(metrics)
        ))
        ## Standardize cell-level filtering args.
        args <- list(
            "minCounts" = minCounts,
            "maxCounts" = maxCounts,
            "minFeatures" = minFeatures,
            "maxFeatures" = maxFeatures,
            "minNovelty" = minNovelty,
            "maxMitoRatio" = maxMitoRatio,
            "nCells" = nCells
        )
        ## Loop across the arguments and expand to match the number of samples,
        ## so we can run parameterized checks via `mapply()`.
        args <- lapply(
            X = args,
            FUN = function(arg) {
                if (hasLength(arg, n = 1L)) {
                    arg <- rep(arg, times = length(sampleIds))
                    names(arg) <- sampleIds
                }
                assert(identical(names(arg), sampleIds))
                arg
            }
        )
        ## Handle the `nCells` argument differentely downstream.
        nCells <- args[["nCells"]]
        args <- args[setdiff(names(args), "nCells")]
        assert(allAreMatchingRegex(x = names(args), pattern = "^(max|min)"))
        ## Determine the relational operator to use based on the name.
        ## Use GTE (`>=`) for `min*` and LTE (`<=`) for `max*`.
        operators <- lapply(
            X = names(args),
            FUN = function(x) {
                if (grepl("^min", x)) {
                    `>=`
                } else if (grepl("^max", x)) {
                    `<=`
                } else {
                    NULL
                }
            }
        )
        names(operators) <- names(args)
        ## Map the cell arguments to the metrics column name.
        arg2col <- c(
            "minCounts" = countsCol,
            "maxCounts" = countsCol,
            "minFeatures" = featuresCol,
            "maxFeatures" = featuresCol,
            "minNovelty" = noveltyCol,
            "maxMitoRatio" = mitoRatioCol
        )
        ## Split the metrics per sample so we can perform parameterized
        ## filtering checks using `mapply()`.
        split <- split(x = metrics, f = metrics[["sampleId"]])
        assert(is(split, "SplitDataFrameList"))
        ## Loop across the samples.
        filter <- DataFrameList(mapply(
            sampleName = names(split),
            metrics = split,
            FUN = function(sampleName, metrics) {
                ## Loop across the filtering parameters (per sample).
                perSample <- mapply(
                    argName = names(args),
                    arg = args,
                    operator = operators,
                    metricCol = arg2col,
                    FUN = function(argName, arg, operator, metricCol) {
                        assert(isSubset(sampleName, names(arg)))
                        e1 <- metrics[[metricCol]]
                        e2 <- arg[[sampleName]]
                        do.call(what = operator, args = list(e1 = e1, e2 = e2))
                    },
                    SIMPLIFY = FALSE,
                    USE.NAMES = TRUE
                )
                perSample <- DataFrame(perSample)
                rownames(perSample) <- rownames(metrics)
                perSample
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        ))
        ## Coerce the filter list to a single sparse logical matrix.
        lgl <- do.call(what = rbind, args = filter)
        lgl <- lgl[colnames(object), , drop = FALSE]
        lgl <- decode(lgl)
        lgl <- as(lgl, "Matrix")
        ## Drop columns that contain all NA.
        keep <- apply(
            X = lgl,
            MARGIN = 2L,
            FUN = function(x) {
                !all(is.na(x))
            }
        )
        lgl <- lgl[, keep, drop = FALSE]
        assert(!isTRUE(anyNA(lgl)))
        cells <- apply(X = lgl, MARGIN = 1L, FUN = function(x) { all(x) })
        assert(identical(names(cells), colnames(object)))
        ## Keep top expected number of cells per sample.
        if (any(nCells < Inf)) {
            metrics <- metrics[cells, , drop = FALSE]
            split <- split(x = metrics, f = metrics[["sampleId"]])
            topCellsPerSample <- mapply(
                metrics = split,
                n = nCells,
                FUN = function(metrics, n) {
                    metric <- decode(metrics[[countsCol]])
                    names(metric) <- rownames(metrics)
                    head(sort(metric, decreasing = TRUE), n = n)
                },
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE
            )
            topCells <- unlist(unname(topCellsPerSample))
            metrics <- metrics[names(topCells), , drop = FALSE]
            cells <- names(cells) %in% names(topCells)
            names(cells) <- colnames(object)
        } else {
            topCellsPerSample <- NULL
        }
        ## Remove the low quality cells.
        if (!any(cells)) {
            stop("No cells passed filtering.")
        } else if (sum(cells, na.rm = TRUE) < ncol(object)) {
            assert(identical(names(cells), colnames(object)))
            object <- object[, cells, drop = FALSE]
        }
        ## Detect low quality features (i.e. genes) ----------------------------
        ## Important: remove the low quality cells prior to this calculation.
        if (minCellsPerFeature > 0L) {
            nonzero <- counts(object) > 0L
            ## Ensure we're performing sparse calculations.
            if (is(nonzero, "Matrix")) {
                rowSums <- Matrix::rowSums
            }
            features <- rowSums(nonzero) >= minCellsPerFeature
        } else {
            features <- rep(TRUE, times = nrow(object))
            names(features) <- rownames(object)
        }
        ## Remove the low quality features.
        if (!any(features)) {
            stop("No features passed filtering.")
        } else if (sum(features, na.rm = TRUE) < nrow(object)) {
            assert(identical(names(features), rownames(object)))
            object <- object[features, , drop = FALSE]
        }
        ## Summary statistics --------------------------------------------------
        assert(
            is.logical(cells),
            is.logical(features)
        )
        nCells <- sum(cells, na.rm = TRUE)
        nFeatures <- sum(features, na.rm = TRUE)
        if (identical(c(nFeatures, nCells), originalDim)) {
            cli_alert_warning("No filtering applied.")
            return(object)
        }
        ## Ensure that there are no all zero rows or columns.
        ## Otherwise, downstream conversion to Seurat can error.
        assert(hasNonzeroRowsAndCols(counts(object)))
        perSamplePass <- lapply(
            X = filter,
            FUN = function(x) {
                x <- vapply(
                    X = x,
                    FUN = function(x) {
                        sum(x, na.rm = FALSE)
                    },
                    FUN.VALUE = numeric(1L)
                )
                x <- na.omit(x)
                ## Drop the `na.omit` attribute, which is annoying in print.
                names <- names(x)
                x <- as.integer(x)
                names(x) <- names
                x
            }
        )
        totalPass <- Matrix::colSums(lgl, na.rm = TRUE)
        storage.mode(totalPass) <- "integer"
        ## Inform the user regarding filtering parameters.
        cli_text("Pre-filter:")
        cli_div(theme = list(body = list("margin-left" = 2L)))
        cli_ul(items = c(
            sprintf(
                fmt = "%d %s",
                originalDim[[2L]],
                ngettext(
                    n = originalDim[[2L]],
                    msg1 = "cell",
                    msg2 = "cells"
                )
            ),
            sprintf(
                fmt = "%d %s",
                originalDim[[1L]],
                ngettext(
                    n = originalDim[[1L]],
                    msg1 = "feature",
                    msg2 = "features"
                )
            )
        ))
        cli_end()
        cli_text("Post-filter:")
        cli_div(theme = list(body = list("margin-left" = 2L)))
        cli_ul(items = c(
            sprintf(
                fmt = "%d %s (%s)",
                nCells,
                ngettext(
                    n = nCells,
                    msg1 = "cell",
                    msg2 = "cells"
                ),
                percent(nCells / originalDim[[2L]])
            ),
            sprintf(
                fmt = "%d %s (%s)",
                nFeatures,
                ngettext(
                    n = nFeatures,
                    msg1 = "feature",
                    msg2 = "features"
                ),
                percent(nFeatures / originalDim[[1L]])
            )
        ))
        cli_end()
        cli_text("Per argument:")
        cli_div(theme = list(body = list("margin-left" = 4L)))
        cli_dl(items = totalPass)
        cli_end()
        if (length(perSamplePass) > 1L) {
            cli_text("Per sample, per argument:")
            for (i in seq_along(perSamplePass)) {
                cli_div(theme = list(body = list("margin-left" = 2L)))
                cli_text(sprintf("%s:", names(perSamplePass)[[i]]))
                cli_end()
                cli_div(theme = list(body = list("margin-left" = 4L)))
                cli_dl(items = perSamplePass[[i]])
                cli_end()
            }
            ## > cli_verbatim(printString(perSamplePass))
        }
        ## Update object -------------------------------------------------------
        metadata <- SimpleList(
            cells = cells,
            features = features,
            topCellsPerSample = topCellsPerSample,
            args = args,
            filter = filter,
            perSamplePass = perSamplePass,
            totalPass = totalPass,
            call = standardizeCall(),
            date = Sys.Date(),
            version = .version
        )
        metadata <- Filter(f = Negate(is.null), x = metadata)
        metadata(object)[["filterCells"]] <- metadata
        metadata(object)[["subset"]] <- TRUE
        cli_alert_success("Cell filtering was successful.")
        object
    }



#' @rdname filterCells
#' @export
setMethod(
    f = "filterCells",
    signature = signature("SingleCellExperiment"),
    definition = `filterCells,SingleCellExperiment`
)
