#' Plot Heatmap with Quantile Breaks
#'
#' @rdname plotQuantileHeatmap
#' @name plotQuantileHeatmap
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inheritParams general
#' @inheritParams plotHeatmap
#'
#' @param n The number of breaks to create.
#'
#' @return Show heatmap. Invisibly return [list] containing breaks and
#'   [pheatmap::pheatmap()] `gtable`.
#'
#' @examples
#' mat <- matrix(1:10000, nrow = 100, ncol = 100)
#' plotQuantileHeatmap(mat)
NULL



# Constructors =================================================================
#' Create Breaks Based on Quantiles of the Data
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom stats quantile
#'
#' @param x Numeric vector.
#' @param n The number of breaks to create.
#'
#' @return A vector of `n` quantile breaks.
.quantileBreaks <- function(object, n = 5L) {
    assert_is_matrix(object)
    assert_is_integer(n)
    assert_is_scalar(n)
    assert_all_are_positive(n)
    q <- quantile(object, probs = seq(0L, 1L, length.out = n))
    q[!duplicated(q)]
}



#' Quantile Heatmap Constructor
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dendsort dendsort
#' @importFrom grDevices colorRampPalette
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats dist hclust
#' @importFrom viridis viridis
.plotQuantileHeatmap <- function(
    object,
    n = 5L,
    annotationCol = NA,
    clusterCols = TRUE,
    clusterRows = TRUE,
    color = viridis::viridis,
    legendColor = viridis::viridis,
    title = NULL) {
    # Passthrough: n
    assert_has_dims(object)
    assert_all_are_greater_than(nrow(object), 1L)
    assert_all_are_greater_than(ncol(object), 1L)
    object <- as.matrix(object)
    assert_formal_annotation_col(object, annotationCol)
    assert_is_a_bool(clusterCols)
    assert_is_a_bool(clusterRows)
    assert_formal_color_function(color)
    assert_formal_color_function(legendColor)
    assert_is_a_string_or_null(title)

    # Calculate the quantile breaks
    breaks <- .quantileBreaks(object, n = n)

    # FIXME This code is shared with `plotHeatmap()`...generalize
    # Prepare the annotation columns, if necessary. Check for `dim()` here
    # so we can support input of `DataFrame` class objects.
    if (is.data.frame(annotationCol)) {
        annotationCol <- annotationCol %>%
            .[colnames(object), , drop = FALSE] %>%
            rownames_to_column() %>%
            mutate_all(factor) %>%
            column_to_rownames()
    }

    # FIXME This code can also be generalized...
    # Define colors for each annotation column, if desired
    if (is.data.frame(annotationCol) & is.function(legendColor)) {
        annotationColors <- lapply(
            X = seq_along(colnames(annotationCol)),
            FUN = function(a) {
                col <- annotationCol[[a]] %>%
                    levels()
                colors <- annotationCol[[a]] %>%
                    levels() %>%
                    length() %>%
                    legendColor
                names(colors) <- col
                colors
            }) %>%
            setNames(colnames(annotationCol))
    } else {
        annotationColors <- NULL
    }

    if (is.function(color)) {
        color <- color(length(breaks) - 1L)
    } else {
        color <- rev(brewer.pal(n = 7L, name = "RdYlBu"))
        color <- colorRampPalette(color)(length(breaks) - 1L)
    }

    # Dynamic column and row labeling
    if (ncol(object) <= 50L) {
        showColnames <- TRUE
    } else{
        showColnames <- FALSE
    }
    if (nrow(object) <= 50L) {
        showRownames <- TRUE
    } else {
        showRownames <- FALSE
    }

    if (!is.character(title)) {
        title <- ""
    }

    p <- pheatmap(
        mat = object,
        annotation_col = annotationCol,
        annotation_colors = annotationColors,
        border_color = NA,
        breaks = breaks,
        cluster_cols = clusterCols,
        cluster_rows = clusterRows,
        color = color,
        main = title,
        show_colnames = showColnames,
        show_rownames = showRownames)
    p

    list <- list(
        quantiles = breaks,
        plot = p
    )
    invisible(list)
}



# Methods ======================================================================
#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    "plotQuantileHeatmap",
    signature("dgCMatrix"),
    .plotQuantileHeatmap)



#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    "plotQuantileHeatmap",
    signature("matrix"),
    .plotQuantileHeatmap)
