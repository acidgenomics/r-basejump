#' Plot Heatmap
#'
#' Construct a simple heatmap. By default, row- and column-wise hierarchical
#' clustering is performed using the Ward method, but this behavior can be
#' overrided by setting `cluster_rows` or `cluster_cols` to `FALSE`.
#'
#' @rdname plotHeatmap
#' @name plotHeatmap
#'
#' @inheritParams AllGenerics
#'
#' @param scale Character indicating if the values should be centered and scaled
#'   in either the row direction or the column direction, or none. Corresponding
#'   values are "row", "column" and "none".
#' @param annotationCol *Optional*. [data.frame] that defines annotation
#'   mappings for the columns.
#' @param clusterCols Logical determining if columns should be arranged with
#'   hierarchical clustering. Alternatively, can define an `hclust` object.
#' @param clusterRows Logical determining if rows should be arranged with
#'   hierarchical clustering. Alternatively, can define an `hclust` object.
#' @param color Colors to use for plot. Defaults to the [viridis::viridis()]
#'   palette.
#' @param legendColor Colors to use for legend labels. Defaults to the
#'   [viridis::viridis()] palette.
#' @param title *Optional*. Plot title.
#' @param ... Passthrough arguments to [pheatmap::pheatmap()].
#'
#' @seealso [pheatmap::pheatmap()].
#'
#' @return [pheatmap::pheatmap()] return [list], containing `gtable`.
#'
#' @examples
#' mat <- as.matrix(mtcars)
#' plotHeatmap(mat)
NULL



# Constructors =================================================================
#' @importFrom dplyr mutate_all
#' @importFrom grDevices colorRampPalette
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats setNames
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom viridis viridis
.plotHeatmap <- function(
    object,
    scale = "row",
    annotationCol = NA,
    clusterCols = TRUE,
    clusterRows = TRUE,
    color = viridis::viridis(256L),
    legendColor = viridis::viridis,
    title = NULL,
    ...) {
    # Parameter integrity checks ===============================================
    # Passthrough: clusterCols, clusterRows
    .checkScale(scale)
    .checkAnnotationCol(annotationCol)
    .checkColorVector(color)
    .checkColorFunction(legendColor)
    .checkTitle(title)

    # Drop rows that are all zero, when row scaling is applied
    if (scale == "row") {
        object <- object %>%
            .[rowSums(.) > 0L, , drop = FALSE]
    }

    if (nrow(object) < 2L) {
        abort("Need at least 2 rows to plot heatmap")
    }
    if (ncol(object) < 2L) {
        abort("Need at least 2 columns to plot heatmap")
    }

    # Prepare the annotation columns, if necessary. Check for `dim()` here
    # so we can support input of `DataFrame` class objects.
    if (!is.null(dim(annotationCol))) {
        annotationCol <- annotationCol %>%
            as.data.frame() %>%
            .[colnames(object), , drop = FALSE] %>%
            rownames_to_column() %>%
            mutate_all(factor) %>%
            column_to_rownames()
    } else {
        annotationCol <- NA
    }

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

    # If `color = NULL`, use the pheatmap default
    if (!is.character(color)) {
        color <- colorRampPalette(rev(
            brewer.pal(n = 7L, name = "RdYlBu")
        ))(100L)
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

    # pheatmap will error if `NULL` title is passed as `main`
    if (is.null(title)) {
        title <- ""
    }

    pheatmap(
        mat = object,
        annotation_col = annotationCol,
        annotation_colors = annotationColors,
        border_color = NA,
        cluster_cols = clusterCols,
        cluster_rows = clusterRows,
        color = color,
        main = title,
        scale = scale,
        show_colnames = showColnames,
        show_rownames = showRownames,
        ...)
}



# Methods ======================================================================
#' @rdname plotHeatmap
#' @importFrom viridis viridis
#' @export
setMethod(
    "plotHeatmap",
    signature("matrix"),
    .plotHeatmap)
