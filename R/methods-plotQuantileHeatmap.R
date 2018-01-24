#' Plot Heatmap with Quantile Breaks
#'
#' @rdname plotQuantileHeatmap
#' @name plotQuantileHeatmap
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inheritParams AllGenerics
#' @inheritParams plotHeatmap
#'
#' @param n The number of breaks to create.
#'
#' @return Show heatmap. Invisibly return [list] containing breaks and
#'   [pheatmap::pheatmap()] `gtable`.
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
.quantileBreaks <- function(x, n = 5L) {
    q <- quantile(x, probs = seq(0L, 1L, length.out = n))
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
    object <- as.matrix(object)

    if (nrow(object) < 2L) {
        abort("Need at least 2 rows to plot heatmap")
    }
    if (ncol(object) < 2L) {
        abort("Need at least 2 columns to plot heatmap")
    }

    # Calculate the quantile breaks
    breaks <- .quantileBreaks(object, n = n)
    print(format(breaks, digits = 3L))

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
            seq_along(colnames(annotationCol)), function(a) {
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
