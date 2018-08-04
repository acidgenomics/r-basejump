#' Plot Heatmap
#'
#' Construct a simple heatmap. By default, row- and column-wise hierarchical
#' clustering is performed using the Ward method, but this behavior can be
#' overrided by setting `clusterRows` or `clusterCols` to `FALSE`.
#'
#' @name plotHeatmap
#' @family Plot Functions
#'
#' @inheritParams general
#' @param scale `string`. Whether the values should be centered and scaled in
#'   either the row or column direction ("`row`", "`column`"), or remain
#'   unscaled ("`none`").
#' @param clusterRows,clusterCols `boolean`. Arrange with hierarchical
#'   clustering.
#' @param showRownames,showColnames `boolean`. Show row or column names.
#' @param treeheightRow,treeheightCol `scalar integer`. Size of the row and
#'   column dendrograms. Use `0` to disable.
#' @param color `function`, `character`, or `NULL`. Hexadecimal color function
#'   or values to use for plot. We generally recommend these hexadecimal
#'   functions from the viridis package:
#'   - [viridis::viridis()] (*default*).
#'   - [viridis::inferno()].
#'   - [viridis::magma()].
#'   - [viridis::plasma()].
#'   Alternatively, colors can be defined manually using hexadecimal values
#'   (e.g. `c("#FF0000", "#0000FF")`), but this is not generally recommended.
#'   Refer to the RColorBrewer package for hexadecimal color palettes that may
#'   be suitable. If set `NULL`, will use the default pheatmap colors.
#' @param legendColor `function` or `NULL`. Hexadecimal color function to use
#'   for legend labels. Note that hexadecimal values are not supported. If set
#'   `NULL`, will use the default pheatmap colors.
#' @param borderColor `string` or `NULL`. Border color. Disabled by default for
#'   improved aesthetics.
#' @param title `string` or `NULL`. Plot title.
#' @param ... Passthrough arguments to [pheatmap::pheatmap()]. The names of the
#'   arguments should be formatted in camel case, not snake case.
#'
#' @seealso
#' - [pheatmap::pheatmap()].
#' - [RColorBrewer::brewer.pal()].
#'
#' @return Show heatmap and invisibly return a `list` of the components.
#'
#' @examples
#' # SummarizedExperiment ====
#' plotHeatmap(rse_dds)
#'
#' # Set legend using interesting groups, and customize colors
#' plotHeatmap(
#'     object = rse_dds,
#'     interestingGroups = "condition",
#'     color = plasma,
#'     legendColor = viridis
#' )
#'
#' # Hexadecimal color input
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotHeatmap(rse_dds, color = purple_orange)
#'
#' # Default pheatmap colors
#' plotHeatmap(rse_dds, color = NULL)
#'
#' # Disable column clustering
#' plotHeatmap(rse_dds, clusterCols = FALSE)
NULL



# Methods ======================================================================
#' @rdname plotHeatmap
#' @export
setMethod(
    "plotHeatmap",
    signature("SummarizedExperiment"),
    function(
        object,
        interestingGroups,
        scale = c("row", "column", "none"),
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
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
        scale <- match.arg(scale)
        assert_is_a_bool(clusterCols)
        assert_is_a_bool(clusterRows)
        assert_is_a_bool(showColnames)
        assert_is_a_bool(showRownames)
        assert_is_a_number(treeheightRow)
        assert_is_a_number(treeheightCol)
        assert_all_are_non_negative(treeheightRow, treeheightCol)
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
        if (scale == "row") {
            # Filter out any zero count rows
            mat <- mat[rowSums(mat) > 0L, , drop = FALSE]
        }

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
            clusterCols = clusterCols,
            clusterRows = clusterRows,
            color = color,
            main = title,
            scale = scale,
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
