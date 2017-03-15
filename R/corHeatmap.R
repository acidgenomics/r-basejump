#' Correlation matrix heatmap
#' 
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#' 
#' @import pheatmap
#' 
#' @param counts Counts matrix
#' 
#' @return Heatmap
#' @export
corHeatmap <- function(counts,
                       method = "pearson",
                       annotation,
                       ...) {
    counts %>%
        cor(method = method) %>%
        pheatmap::pheatmap(main = paste(method, "correlation"),
                           annotation = annotation,
                           ...)
}
