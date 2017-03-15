#' Correlation matrix heatmap
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import pheatmap
#'
#' @param counts Counts matrix
#' @param method Correlation coefficient (or covariance) to be computed
#' @param annotation Annotation data frame
#' @param ... Passthrough to \code{pheatmap()}
#'
#' @return Heatmap
#' @export
corHeatmap <- function(counts,
                       method = "pearson",
                       annotation,
                       ...) {
    if (!is.matrix(counts)) {
        stop("A counts matrix is required.")
    }
    if (!is.data.frame(annotation)) {
        stop("An annotation data.frame is required.")
    }
    counts %>%
        cor(method = method) %>%
        pheatmap::pheatmap(main = paste(method, "correlation"),
                           annotation = annotation,
                           ...)
}
