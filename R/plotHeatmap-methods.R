# FIXME Improve the documentation here. Need to explain why correlation and
# quantile heatmaps are useful.
# TODO Consider erroring or prompting the user for really large datasets.



#' Plot Heatmap
#'
#' Construct a simple heatmap.
#'
#' @section Hierarchical clustering:
#'
#' By default, row- and column-wise hierarchical clustering is performed using
#' the Ward method, but this behavior can be overrided by setting `clusterRows`
#' or `clusterCols` to `FALSE`.
#'
#' @section Correlation heatmap:
#'
#' Construct a correlation heatmap comparing the columns of the matrix.
#'
#' @section Quantile heatmap:
#'
#' Apply quantile breaks.
#'
#' @name plotHeatmap
#' @family Plot Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams general
#'
#' @param borderColor `string` or `NULL`. Border color. Disabled by default for
#'   improved aesthetics.
#' @param clusteringMethod `string`. Clustering method. Accepts the same values
#'   as [stats::hclust()].
#' @param clusterRows,clusterCols `boolean`. Arrange with hierarchical
#'   clustering.
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
#' @param legend `boolean`. Show the color legend.
#' @param legendColor `function` or `NULL`. Hexadecimal color function to use
#'   for legend labels. Note that hexadecimal values are not supported. If set
#'   `NULL`, will use the default pheatmap colors.
#' @param method `string`. Correlation coefficient (or covariance) method to be
#'   computed. Defaults to "`pearson`" but "`spearman`" can also be used.
#'   Consult the [stats::cor()] documentation for more information.
#' @param n `scalar integer`. The number of quantile breaks to create.
#' @param scale `string`. Whether the values should be centered and scaled in
#'   either the row or column direction ("`row`", "`column`"), or remain
#'   unscaled ("`none`").
#' @param showRownames,showColnames `boolean`. Show row or column names.
#' @param treeheightRow,treeheightCol `scalar integer`. Size of the row and
#'   column dendrograms. Use `0` to disable.
#' @param title `string` or `NULL`. Plot title.
#' @param ... Passthrough arguments to [pheatmap::pheatmap()]. The names of the
#'   arguments should be formatted in camel case, not snake case.
#'
#' @seealso
#' - [pheatmap::pheatmap()].
#' - [RColorBrewer::brewer.pal()].
#'
#' @return `pheatmap`.
#'
#' @seealso
#' - [pheatmap::pheatmap()].
#' - [stats::cor()].
#' - [stats::hclust()].
#'
#' @examples
#' # SummarizedExperiment ====
#' plotHeatmap(rse_small)
#' plotCorrelationHeatmap(rse_small)
#' plotQuantileHeatmap(rse_small)
#'
#' # Disable column clustering.
#' plotHeatmap(rse_small, clusterCols = FALSE)
#'
#' # Using pheatmap default colors.
#' plotHeatmap(rse_small, color = NULL, legendColor = NULL)
#'
#' # Using hexadecimal color input.
#' library(RColorBrewer)
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotHeatmap(rse_small, color = purple_orange)
#'
#' # SingleCellExperiment ====
#' plotHeatmap(sce_small)
#' plotCorrelationHeatmap(sce_small)
#' plotQuantileHeatmap(sce_small)
NULL



# SummarizedExperiment =========================================================
.plotHeatmap.SE <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        scale = c("none", "row", "column"),
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = viridis::viridis,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        if (has_length(interestingGroups)) {
            interestingGroups(object) <- interestingGroups
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

        # Convert the SE object to use symbols in the rownames, for pheatmap.
        object <- convertGenesToSymbols(object)

        # Ensure we're using a dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Filter out any zero count rows when row scaling, otherwise hclust
        # calculation will error.
        if (scale == "row") {
            mat <- mat[rowSums(mat) > 0L, , drop = FALSE]
        }

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
        if (has_length(sampleNames)) {
            colnames(mat) <- sampleNames
            if (
                has_length(annotationCol) &&
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
        do.call(what = pheatmap, args = args)
    }



.plotCorrelationHeatmap.SE <-  # nolint
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
        assert_is_scalar(assay)
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        if (has_length(interestingGroups)) {
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
        mat <- as.matrix(assays(object)[[assay]])
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
        if (has_length(sampleNames)) {
            rownames(mat) <- sampleNames
            colnames(mat) <- sampleNames
            if (
                has_length(annotationCol) &&
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
        assert_are_disjoint_sets(
            x = names(args),
            y = "scale"
        )
        do.call(what = pheatmap, args = args)
    }



.quantileBreaks <- function(object, n = 10L) {
    assert_is_matrix(object)
    assert_is_an_integer(n)
    assert_all_are_positive(n)
    breaks <- quantile(object, probs = seq(0L, 1L, length.out = n))
    breaks[!duplicated(breaks)]
}



.plotQuantileHeatmap.SE <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        n = 10L,
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = viridis::viridis,
        legend = FALSE,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert_all_are_greater_than(nrow(object), 1L)
        assert_all_are_greater_than(ncol(object), 1L)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        if (has_length(interestingGroups)) {
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
        mat <- as.matrix(assays(object)[[assay]])

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
        if (has_length(sampleNames)) {
            colnames(mat) <- sampleNames
            if (
                has_length(annotationCol) &&
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



# SingleCellExperiment =========================================================
# FIXME Think about default calculation here. Mean?
.plotHeatmap.SCE <-  # nolint
    function(object) {
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(.plotHeatmap.SCE) <- formals(.plotHeatmap.SE)



# FIXME Think about the default calculation here. Sum?
.plotCorrelationHeatmap.SCE <-  # nolint
    function(object) {
        do.call(
            what = plotCorrelationHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(.plotCorrelationHeatmap.SCE) <- formals(.plotCorrelationHeatmap.SE)



# FIXME Think about default calculation here. Mean?
.plotQuantileHeatmap.SCE <-  # nolint
    function(object) {
        do.call(
            what = plotQuantileHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(.plotQuantileHeatmap.SCE) <- formals(.plotQuantileHeatmap.SE)



# Methods ======================================================================
#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = .plotHeatmap.SE
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = .plotHeatmap.SCE
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = .plotCorrelationHeatmap.SE
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = .plotCorrelationHeatmap.SCE
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = .plotQuantileHeatmap.SE
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = .plotQuantileHeatmap.SCE
)
