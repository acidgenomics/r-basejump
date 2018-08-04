#' Plot Heatmap with Quantile Breaks
#'
#' @name plotQuantileHeatmap
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inherit plotHeatmap
#' @param n `scalar integer`. The number of breaks to create.
#' @param legend `boolean`. Show the color legend.
#'
#' @examples
#' # SummarizedExperiment ====
#' plotQuantileHeatmap(rse_dds)
#'
#' # Set legend using interesting groups, and customize colors
#' plotQuantileHeatmap(
#'     object = rse_dds,
#'     interestingGroups = "condition",
#'     color = plasma,
#'     legendColor = viridis
#' )
NULL



# Constructors =================================================================
#' Create Breaks Based on Quantiles of the Data
#'
#' @keywords internal
#' @noRd
#'
#' @param x Numeric vector.
#' @param n The number of breaks to create.
#'
#' @return A vector of `n` quantile breaks.
.quantileBreaks <- function(object, n = 10L) {
    assert_is_matrix(object)
    assert_is_an_integer(n)
    assert_all_are_positive(n)
    breaks <- quantile(object, probs = seq(0L, 1L, length.out = n))
    breaks[!duplicated(breaks)]
}



#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    "plotQuantileHeatmap",
    signature("SummarizedExperiment"),
    function(
        object,
        interestingGroups,
        n = 10L,
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        legend = FALSE,
        color = viridis,
        legendColor = NULL,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        if (missing(interestingGroups)) {
            interestingGroups <- basejump::interestingGroups(object)
        }
        assertIsAnImplicitInteger(n)
        n <- as.integer(n)
        assert_is_a_bool(clusterCols)
        assert_is_a_bool(clusterRows)
        assert_is_a_bool(legend)
        assertIsAStringOrNULL(borderColor)
        if (!is_a_string(borderColor)) {
            borderColor <- NA
        }
        assertIsAStringOrNULL(title)
        if (!is_a_string(title)) {
            title <- NA
        }

        object <- suppressWarnings(convertGenesToSymbols(object))
        mat <- as.matrix(assay(object))

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

        # Calculate the quantile breaks
        breaks <- .quantileBreaks(mat, n = n)

        annotationCol <- .pheatmapAnnotationCol(annotationCol)
        annotationColors <- .pheatmapAnnotationColors(
            annotationCol = annotationCol,
            legendColor = legendColor
        )
        color <- .pheatmapColor(color, n = length(breaks) - 1L)

        # Return pretty heatmap with modified defaults
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            breaks = breaks,
            clusterCols = clusterCols,
            clusterRows = clusterRows,
            color = color,
            legend = legend,
            legendBreaks = breaks,
            legendLabels = round(breaks, digits = 2L),
            main = title,
            scale = "none",
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
