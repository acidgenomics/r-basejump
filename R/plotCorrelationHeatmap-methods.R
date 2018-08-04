#' Plot Correlation Heatmap
#'
#' Construct a correlation heatmap comparing the columns of the matrix.
#'
#' @name plotCorrelationHeatmap
#' @family Plot Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams plotHeatmap
#' @param method `string`. Correlation coefficient (or covariance) method to be
#'   computed. Defaults to "`pearson`" but "`spearman`" can also be used.
#'   Consult the [stats::cor()] documentation for more information.
#' @param clusteringMethod `string`. Clustering method. Accepts the same values
#'   as [stats::hclust()].
#'
#' @seealso
#' - [stats::cor()].
#' - [stats::hclust()].
#' - [pheatmap::pheatmap()].
#'
#' @return Show heatmap and invisibly return a `list` of the components.
#'
#' @examples
#' # SummarizedExperiment ====
#' plotCorrelationHeatmap(rse_dds)
#'
#' # Set legend using interesting groups, and customize colors
#' plotCorrelationHeatmap(
#'     object = rse_dds,
#'     interestingGroups = "condition",
#'     color = plasma,
#'     legendColor = viridis
#' )
#'
#' # Hexadecimal color input
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotCorrelationHeatmap(rse_dds, color = purple_orange)
#'
#' # Default pheatmap colors
#' plotCorrelationHeatmap(rse_dds, color = NULL)
NULL



# Methods ======================================================================
#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    "plotCorrelationHeatmap",
    signature("SummarizedExperiment"),
    function(
        object,
        interestingGroups,
        method = c("pearson", "spearman"),
        clusteringMethod = "ward.D2",
        showRownames = TRUE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis,
        legendColor = NULL,
        borderColor = NULL,
        title = TRUE,
        ...
    ) {
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        if (missing(interestingGroups)) {
            interestingGroups <- basejump::interestingGroups(object)
        }
        method <- match.arg(method)
        assert_is_a_string(clusteringMethod)
        assert_is_a_bool(showColnames)
        assert_is_a_bool(showRownames)
        assert_is_a_number(treeheightRow)
        assert_is_a_number(treeheightCol)
        assert_all_are_non_negative(treeheightRow, treeheightCol)
        assertIsAStringOrNULL(borderColor)
        if (!is_a_string(borderColor)) {
            borderColor <- NA
        }
        if (isTRUE(title)) {
            title <- paste(method, "correlation")
        } else if (!is_a_string(title)) {
            title <- NA
        }

        # Correlation matrix
        mat <- as.matrix(assay(object))
        mat <- cor(mat, method = method)

        if (length(interestingGroups)) {
            annotationCol <- colData(object)[, interestingGroups, drop = FALSE]
        } else {
            annotationCol <- NULL
        }

        # Use `sampleName`, if defined
        sampleName <- colData(object)[["sampleName"]]
        if (length(sampleName)) {
            colnames(mat) <- sampleName
            if (length(annotationCol)) {
                rownames(annotationCol) <- sampleName
            }
        }

        annotationCol <- .pheatmapAnnotationCol(annotationCol)
        annotationColors <- .pheatmapAnnotationColors(
            annotationCol = annotationCol,
            legendColor = legendColor
        )
        color <- .pheatmapColor(color)

        # Return pretty heatmap with modified defaults
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
        do.call(pheatmap, args)
    }
)
