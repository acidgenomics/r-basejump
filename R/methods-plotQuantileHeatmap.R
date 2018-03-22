#' Plot Heatmap with Quantile Breaks
#'
#' @name plotQuantileHeatmap
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inherit plotHeatmap
#'
#' @param n The number of breaks to create.
#'
#' @return Show heatmap. Invisibly return `list` containing breaks and `gtable`.
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
#' @param x Numeric vector.
#' @param n The number of breaks to create.
#'
#' @return A vector of `n` quantile breaks.
.quantileBreaks <- function(object, n = 5L) {
    assert_is_matrix(object)
    assert_is_an_integer(n)
    assert_all_are_positive(n)
    q <- quantile(object, probs = seq(0L, 1L, length.out = n))
    q[!duplicated(q)]
}



.plotQuantileHeatmap <- function(
    object,
    n = 5L,
    annotationCol = NULL,
    clusterCols = TRUE,
    clusterRows = TRUE,
    color = viridis,
    legendColor = viridis,
    borderColor = NULL,
    title = NULL,
    ...
) {
    assert_has_dims(object)
    assert_all_are_greater_than(nrow(object), 1L)
    assert_all_are_greater_than(ncol(object), 1L)
    object <- as.matrix(object)
    assertIsAnImplicitInteger(n)
    assertFormalAnnotationCol(object, annotationCol)
    if (has_dims(annotationCol)) {
        annotationCol <- as.data.frame(annotationCol)
    }
    assert_is_a_bool(clusterCols)
    assert_is_a_bool(clusterRows)
    assertIsHexColorFunctionOrNULL(color)
    assertIsHexColorFunctionOrNULL(legendColor)
    assertIsAStringOrNULL(title)

    # Calculate the quantile breaks
    breaks <- .quantileBreaks(object, n = n)

    # Prepare the annotation columns, if necessary. Check for `dim()` here
    # so we can support input of `DataFrame` class objects.
    if (is.data.frame(annotationCol)) {
        annotationCol <- annotationCol %>%
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
            }
        ) %>%
            set_names(colnames(annotationCol))
    } else {
        annotationColors <- NULL
    }

    if (is.function(color)) {
        color <- color(length(breaks) - 1L)
    } else {
        color <- rev(brewer.pal(n = 7L, name = "RdYlBu"))
        color <- colorRampPalette(color)(length(breaks) - 1L)
    }

    if (is.null(borderColor)) {
        borderColor <- NA
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

    # Return pretty heatmap with modified defaults
    args <- list(
        "mat" = object,
        "annotationCol" = annotationCol,
        "annotationColors" = annotationColors,
        "borderColor" = borderColor,
        "breaks" = breaks,
        "clusterCols" = clusterCols,
        "clusterRows" = clusterRows,
        "color" = color,
        "main" = title,
        "showColnames" = showColnames,
        "showRownames" = showRownames,
        ...
    )
    # Sanitize all argument names into snake case
    names(args) <- snake(names(args))
    assert_is_subset(names(args), formalArgs(pheatmap))
    p <- do.call(pheatmap, args)
    p

    invisible(list(
        quantiles = breaks,
        plot = p
    ))
}



# Methods ======================================================================
#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    "plotQuantileHeatmap",
    signature("dgCMatrix"),
    .plotQuantileHeatmap
)



#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    "plotQuantileHeatmap",
    signature("matrix"),
    .plotQuantileHeatmap
)
