#' @name plotHeatmap
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @section Hierarchical clustering:
#'
#' By default, row- and column-wise hierarchical clustering is performed using
#' the Ward method, but this behavior can be overrided by setting `clusterRows`
#' or `clusterCols` to `FALSE`.
#'
#' @inheritParams params
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
#' data(rse, sce)
#'
#' ## SummarizedExperiment ====
#' plotHeatmap(rse)
#' plotCorrelationHeatmap(rse)
#' plotQuantileHeatmap(rse)
#'
#' ## Disable column clustering.
#' plotHeatmap(rse, clusterCols = FALSE)
#'
#' ## Using pheatmap default colors.
#' plotHeatmap(rse, color = NULL, legendColor = NULL)
#'
#' ## Using hexadecimal color input.
#' library(RColorBrewer)
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotHeatmap(rse, color = purple_orange)
#'
#' ## SingleCellExperiment ====
#' plotHeatmap(sce)
#' plotCorrelationHeatmap(sce)
#' plotQuantileHeatmap(sce)
NULL



# SummarizedExperiment =========================================================
plotHeatmap.SummarizedExperiment <-  # nolint
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
        assertIsStringOrNULL(borderColor)
        if (!is_a_string(borderColor)) {
            borderColor <- NA
        }
        assertIsStringOrNULL(title)
        if (!is_a_string(title)) {
            title <- NA
        }

        # Convert the SE object to use symbols in the rownames, for pheatmap.
        suppressMessages(
            object <- convertGenesToSymbols(object)
        )

        # Ensure we're always using a dense matrix.
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
        assertIsStringOrNULL(borderColor)
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



plotQuantileHeatmap.SummarizedExperiment <-  # nolint
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
        assertIsStringOrNULL(borderColor)
        if (!is_a_string(borderColor)) {
            borderColor <- NA
        }
        assertIsStringOrNULL(title)
        if (!is_a_string(title)) {
            title <- NA
        }

        # Convert the SE object to use symbols in the rownames, for pheatmap.
        suppressWarnings(
            object <- convertGenesToSymbols(object)
        )

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
plotHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(
            object = object,
            fun = "mean"
        )
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(args = list(object = agg))
        )
    }
formals(plotHeatmap.SingleCellExperiment) <-
    formals(plotHeatmap.SummarizedExperiment)



plotCorrelationHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(
            object = object,
            fun = "mean"
        )
        do.call(
            what = plotCorrelationHeatmap,
            args = matchArgsToDoCall(args = list(object = agg))
        )
    }
formals(plotCorrelationHeatmap.SingleCellExperiment) <-
    formals(plotCorrelationHeatmap.SummarizedExperiment)



plotQuantileHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(
            object = object,
            fun = "mean"
        )
        do.call(
            what = plotQuantileHeatmap,
            args = matchArgsToDoCall(args = list(object = agg))
        )
    }
formals(plotQuantileHeatmap.SingleCellExperiment) <-
    formals(plotQuantileHeatmap.SummarizedExperiment)



# Methods ======================================================================
#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotHeatmap.SummarizedExperiment
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotHeatmap.SingleCellExperiment
)



#' @describeIn plotHeatmap Construct a correlation heatmap comparing the columns
#'   of the matrix.
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotCorrelationHeatmap.SummarizedExperiment
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotCorrelationHeatmap.SingleCellExperiment
)



#' @describeIn plotHeatmap Scale the heatmap by applying quantile breaks.
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotQuantileHeatmap.SummarizedExperiment
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotQuantileHeatmap.SingleCellExperiment
)



# pheatmap =====================================================================
.emptyPheatmapAnnotations <- list(
    annotationCol = NA,
    annotationColors = NA
)



# Automatically handle the annotation data and colors.
# Factors with a single level are automatically dropped.
.pheatmapAnnotations <- function(
    object,
    blacklist = "sampleName",
    legendColor
) {
    validObject(object)
    assert_is_character(blacklist)
    assertIsHexColorFunctionOrNULL(legendColor)

    # Annotation columns -------------------------------------------------------
    data <- colData(object)
    interestingGroups <- interestingGroups(object)

    # pheatmap requires `NA` if empty.
    if (
        !has_dims(data) ||
        !has_length(interestingGroups) ||
        identical(interestingGroups, "sampleName")
    ) {
        return(.emptyPheatmapAnnotations)
    }

    assertHasRownames(data)
    data <- data[, interestingGroups, drop = FALSE]

    # Prepare the blacklist, always excluding sample names from labeling in
    # the pheatmap annotation columns.
    blacklist <- unique(c("sampleName", blacklist))

    data <- data %>%
        as_tibble(rownames = "rowname") %>%
        # Remove blacklisted columns (e.g. `sampleName`).
        .[, setdiff(colnames(.), blacklist), drop = FALSE] %>%
        # Ensure all strings are factors.
        mutate_if(is.character, as.factor) %>%
        # Ensure unwanted columns like `sizeFactor` are dropped.
        select_if(is.factor) %>%
        as.data.frame() %>%
        column_to_rownames("rowname")

    # Drop any remaining factor columns that contain a single level.
    hasLevels <- vapply(
        data,
        FUN = function(x) {
            length(levels(x)) > 1L
        },
        FUN.VALUE = logical(1L)
    )

    # Return empty if there are no useful factor columns.
    if (!has_length(hasLevels)) {
        return(.emptyPheatmapAnnotations)  # nocov
    }

    data <- data[, hasLevels, drop = FALSE]

    # Colors -------------------------------------------------------------------
    if (
        is.data.frame(data) &&
        is.function(legendColor)
    ) {
        colors <- lapply(
            X = data,
            FUN = function(x) {
                assert_is_factor(x)
                levels <- levels(x)
                colors <- legendColor(length(levels))
                names(colors) <- levels
                colors
            })
        names(colors) <- colnames(data)
    } else {
        colors <- NA
    }

    # Return -------------------------------------------------------------------
    list(
        annotationCol = data,
        annotationColors = colors
    )
}



# Sanitize formals into snake case and abort on duplicates.
# Duplicates may arise if user is mixing and matching camel/snake case.
.pheatmapArgs <- function(args) {
    assert_is_list(args)
    assert_has_names(args)
    # Abort on snake case formatted formalArgs
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (has_length(invalidNames)) {
        stop(paste(
            "Specify arguments in camel case:",
            toString(invalidNames)
        ))
    }
    names(args) <- snake(names(args))
    assert_is_subset(names(args), formalArgs(pheatmap))
    assert_has_no_duplicates(names(args))
    args
}



# If `color = NULL`, use the pheatmap default palette
.pheatmapColorPalette <- function(color = NULL, n = 256L) {
    if (is.character(color)) {
        # Hexadecimal color palette
        # (e.g. RColorBrewer palettes)
        assert_all_are_hex_colors(color)
        color
    } else if (is.function(color)) {
        # Hexadecimal color function
        # (e.g. viridis functions)
        assertIsHexColorFunctionOrNULL(color)
        color(n)
    } else {
        # pheatmap default palette
        colorRampPalette(rev(brewer.pal(n = 7L, name = "RdYlBu")))(n)
    }
}
