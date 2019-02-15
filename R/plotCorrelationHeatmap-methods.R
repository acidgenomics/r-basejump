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
        mat <- cor(mat, method = method)

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
                !is.na(annotationCol)
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
