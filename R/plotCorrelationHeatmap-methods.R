#' Plot Correlation Heatmap
#'
#' Construct a correlation heatmap comparing the columns of the matrix.
#'
#' @name plotCorrelationHeatmap
#' @family Plot Functions
#' @author Michael Steinbaugh
#' @export
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
#' plotCorrelationHeatmap(rse_small)
#'
#' # Using viridis color palettes.
#' plotCorrelationHeatmap(
#'     object = rse_small,
#'     color = viridis::plasma,
#'     legendColor = viridis::viridis
#' )
#'
#' # Using hexadecimal color input.
#' library("RColorBrewer")
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotCorrelationHeatmap(rse_small, color = purple_orange)
#'
#' # Using default pheatmap colors.
#' plotCorrelationHeatmap(rse_small, color = NULL)
NULL



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = function(
        object,
        interestingGroups,
        method = c("pearson", "spearman"),
        clusteringMethod = "ward.D2",
        showRownames = TRUE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = NULL,
        borderColor = NULL,
        title = TRUE,
        ...
    ) {
        validObject(object)
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        if (
            is.character(interestingGroups) &&
            !identical(interestingGroups, "sampleName")
        ) {
            interestingGroups(object) <- interestingGroups
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

        # Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(
            object = object,
            legendColor = legendColor
        )
        assert_is_list(x)
        assert_are_identical(
            x = names(x),
            y = c("annotationCol", "annotationColors")
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        rm(x)

        color <- .pheatmapColorPalette(color)

        # Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames)) {
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) &&
                !is.na(annotationCol)
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

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
        do.call(what = pheatmap, args = args)
    }
)
