# TODO Make sure we're stopping if user passes in `scale` argument.



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
#' plotQuantileHeatmap(rse_small)
#'
#' # Using viridis color palettes.
#' plotQuantileHeatmap(
#'     object = rse_small,
#'     color = viridis::plasma,
#'     legendColor = viridis::viridis
#' )
NULL



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
    f = "plotQuantileHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = function(
        object,
        interestingGroups = NULL,
        n = 10L,
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legend = FALSE,
        legendColor = NULL,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        if (length(interestingGroups)) {
            interestingGroups(object) <- interestingGroups
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

        # Convert the SE objec to use symbols in the rownames, for pheatmap.
        object <- suppressWarnings(convertGenesToSymbols(object))

        # Ensure we're using a dense matrix.
        mat <- as.matrix(assay(object))

        # Calculate the quantile breaks.
        breaks <- .quantileBreaks(mat, n = n)

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

        # Note the number of breaks here.
        color <- .pheatmapColorPalette(color, n = length(breaks) - 1L)

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

        # Return pretty heatmap with modified defaults.
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
        do.call(what = pheatmap, args = args)
    }
)
