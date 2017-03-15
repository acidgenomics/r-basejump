#' Counts per gene plot
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import ggplot2
#'
#' @param rawCounts Raw counts \code{matrix}
#' @param metadata Metadata \code{data.frame}
#'
#' @return Boxplot
#' @export
countsPerGene <- function(rawCounts, metadata) {
    rawCounts %>%
        meltLog10(metadata = metadata) %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~description,
                          y = ~counts,
                          color = ~group)
        ) +
        ggplot2::ggtitle("Counts per gene") +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::xlab("sample") +
        ggplot2::ylab(expression(log[10]~counts~per~gene)) +
        ggplot2::coord_flip()
}
