#' Plot Correlation Heatmap
#'
#' Construct a correlation heatmap comparing the columns of the matrix.
#'
#' @name plotCorrelationHeatmap
#' @family Plot Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams plotHeatmap
#'
#' @param method Correlation coefficient (or covariance) method to be computed.
#'   Defaults to "`pearson`" but "`spearman`" can also be used. Consult the
#'   [stats::cor()] documentation for more information.
#' @param clusteringMethod Clustering method. Accepts the same values as
#'   [stats::hclust()].
#'
#' @seealso
#' - [stats::cor()].
#' - [stats::hclust()].
#' - [pheatmap::pheatmap()].
#'
#' @return Show heatmap and return `list`, containing `gtable`.
#'
#' @examples
#' mat <- as.matrix(mtcars)
#' plotCorrelationHeatmap(mat)
NULL



# Constructors =================================================================
.plotCorrelationHeatmap <- function(
    object,
    method = c("pearson", "spearman"),
    clusteringMethod = "ward.D2",
    annotationCol = NULL,
    color = viridis,
    legendColor = viridis,
    borderColor = NULL,
    title = TRUE,
    ...
) {
    assert_has_dims(object)
    assert_all_are_greater_than(nrow(object), 1L)
    assert_all_are_greater_than(ncol(object), 1L)
    method <- match.arg(method)
    assertFormalAnnotationCol(object, annotationCol)
    if (has_dims(annotationCol)) {
        annotationCol <- as.data.frame(annotationCol)
    }
    assertIsHexColorFunctionOrNULL(color)
    assertIsHexColorFunctionOrNULL(legendColor)

    # Title
    if (isTRUE(title)) {
        title <- paste(method, "correlation")
    } else if (!is_a_string(title)) {
        title <- NULL
    }

    # Correlation matrix
    mat <- cor(object, method = method)

    # Prepare the annotation columns
    if (is.data.frame(annotationCol)) {
        # Coerce annotation columns to factors
        annotationCol <- annotationCol %>%
            rownames_to_column() %>%
            mutate_all(factor) %>%
            column_to_rownames()
    }

    # Define colors for each annotation column, if desired
    if (is.data.frame(annotationCol) && is.function(legendColor)) {
        annotationColors <- lapply(
            seq_along(colnames(annotationCol)), function(a) {
                col <- levels(annotationCol[[a]])
                colors <- annotationCol[[a]] %>%
                    levels() %>%
                    length() %>%
                    legendColor()
                names(colors) <- col
                colors
            }) %>%
            set_names(colnames(annotationCol))
    } else {
        annotationColors <- NULL
    }

    # If `color = NULL`, use the pheatmap default
    if (is.function(color)) {
        color <- color(256L)
    } else if (!is.character(color)) {
        color <- colorRampPalette(rev(
            brewer.pal(n = 7L, name = "RdYlBu")
        ))(100L)
    }

    if (is.null(borderColor)) {
        borderColor <- NA
    }

    # Return pretty heatmap with modified defaults
    args <- list(
        "mat" = mat,
        "annotationCol" = annotationCol,
        "annotationColors" = annotationColors,
        "borderColor" = borderColor,
        "clusteringMethod" = clusteringMethod,
        "clusteringDistanceRows" = "correlation",
        "clusteringDistanceCols" = "correlation",
        "color" = color,
        "main" = title,
        "showColnames" = TRUE,
        "showRownames" = TRUE,
        ...
    )
    # Sanitize all argument names into snake case
    names(args) <- snake(names(args))
    assert_is_subset(names(args), formalArgs(pheatmap))

    do.call(pheatmap, args)
}



# Methods ======================================================================
#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    "plotCorrelationHeatmap",
    signature("matrix"),
    .plotCorrelationHeatmap
)
