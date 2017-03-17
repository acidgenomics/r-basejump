#' Correlation matrix heatmap
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import pheatmap
#' @import RColorBrewer
#' @importFrom grDevices colorRampPalette
#'
#' @param counts RNA-Seq counts
#' @param metadata Metadata data frame
#' @param intgroup Internal groups to use for clustering color headers
#' @param method Correlation coefficient (or covariance) to be computed
#' @param format Format of input count data
#' @param ... Passthrough to \code{pheatmap()}
#'
#' @return Heatmap
#' @export
corHeatmap <- function(counts,
                       metadata,
                       intgroup = "group",
                       method = "pearson",
                       format = "DESeqTransform",
                       ...) {
    name <- deparse(substitute(counts))
    if (!is.data.frame(metadata)) {
        stop("A metadata data frame is required.")
    }
    if (!is.character(intgroup)) {
        stop("An intgroup character vector is required.")
    }
    if (format == "DESeqTransform") {
        if (class(counts)[1] != "DESeqTransform") {
            stop("Format was delcared as DESeqTransform, but counts are not a
                 DESeqTransform object.")
        }
        counts <- assay(counts)
    }
    if (!is.matrix(counts)) {
        stop("A counts matrix is required.")
    }
    color <-
        grDevices::colorRampPalette(
            RColorBrewer::brewer.pal(n = 9, name = "Blues")
        )(100)
    counts %>%
        cor(method = method) %>%
        pheatmap::pheatmap(main = paste(method,
                                        "correlation:",
                                        name),
                           annotation = metadata[, intgroup],
                           color = color,
                           show_colnames = FALSE,
                           ...)
}
