#' @name plotCorrelationHeatmap
#' @inherit bioverbs::plotCorrelationHeatmap
#' @inheritParams plotHeatmap
#' @inheritParams params
#'
#' @param method `character(1)`.
#'   Correlation coefficient (or covariance) method to be computed. Defaults to
#'   "`pearson`" but "`spearman`" can also be used. Consult the [stats::cor()]
#'   documentation for more information.
#'
#' @return `pheatmap`.
#' @examples
#' data(rse, sce)
#'
#' ## SummarizedExperiment ====
#' plotCorrelationHeatmap(rse)
#'
#' ## SingleCellExperiment ====
#' plotCorrelationHeatmap(sce)
NULL



#' @importFrom bioverbs plotCorrelationHeatmap
#' @aliases NULL
#' @export
bioverbs::plotCorrelationHeatmap



plotCorrelationHeatmap.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        method = c("pearson", "spearman"),
        clusteringMethod = "ward.D2",
        showRownames = TRUE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = viridis::viridis,
        borderColor = NULL,
        title = TRUE,
        ...
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            nrow(object) > 1L,
            ncol(object) > 1L,
            isString(clusteringMethod),
            isFlag(showRownames),
            isFlag(showColnames),
            isInt(treeheightRow),
            isInt(treeheightCol),
            isString(borderColor, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        method <- match.arg(method)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (isTRUE(title)) {
            title <- paste(method, "correlation")
        } else if (!isString(title)) {
            title <- NA
        }
        # Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        # Correlation matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Inform the user if NA values are present.
        # Note that we're including `na.rm` in `rowVars()` and `colVars()` calls
        # below to handle this edge case.
        if (any(is.na(object))) {
            message("NA values detected in matrix.")
        }

        # Ensure we're dropping rows without variation, otherwise the
        # calculation will fail and return NA.
        varThreshold <- 0L

        # Check across the columns first (samples) and fail, if necessary.
        pass <- colVars(mat, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            stop(paste(
                sprintf(ngettext(
                    n = n,
                    msg1 = "%s column doesn't",
                    msg2 = "%s columns don't"
                ), n),
                "have enough variance:",
                toString(colnames(mat)[which(fail)], width = 200L)
            ))
        }

        # Then check across the rows (features) and inform then remove the
        # offenders automatically.
        pass <- rowVars(mat, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            message(paste(
                sprintf(ngettext(
                    n = n,
                    msg1 = "%s row doesn't",
                    msg2 = "%s rows don't"
                ), n),
                "have enough variance:",
                toString(rownames(mat)[which(fail)], width = 200L),
                "Dropping from return."
            ))
            mat <- mat[pass, , drop = FALSE]
        }

        mat <- cor(mat, method = method)

        # Check to see if hierarchical clustering works on our return.
        # Disable otherwise. This isn't handled well currently in pheatmap.
        message("Calculating distance matrix.")
        d <- dist(mat)
        assert(is(d, "dist"))
        hc <- hclust(d = d, method = clusteringMethod)

        # Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(
                x = names(x),
                y = c("annotationCol", "annotationColors")
            )
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        color <- .pheatmapColorPalette(color)

        # Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            rownames(mat) <- sampleNames
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !any(is.na(annotationCol))
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

        # Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            clusteringMethod = clusteringMethod,
            clusteringDistanceRows = "correlation",
            clusteringDistanceCols = "correlation",
            color = color,
            main = title,
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        assert(areDisjointSets(names(args), "scale"))
        do.call(what = pheatmap, args = args)
    }



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotCorrelationHeatmap.SummarizedExperiment
)



plotCorrelationHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotCorrelationHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(plotCorrelationHeatmap.SingleCellExperiment) <-
    formals(plotCorrelationHeatmap.SummarizedExperiment)



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotCorrelationHeatmap.SingleCellExperiment
)
