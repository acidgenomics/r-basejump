#' Counts per gene plot
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import ggplot2
#'
#' @param counts Counts matrix
#' @param metadata Metadata data frame
#' @param flip Flip x and y axes
#' @param print Print plot
#'
#' @return Boxplot
#' @export
countsPerGene <- function(counts,
                          metadata,
                          flip = TRUE,
                          print = TRUE) {
    if (!is.data.frame(metadata)) {
        stop("A metadata data frame is required.")
    }
    countsName <- deparse(substitute(counts))
    plot <- counts %>%
        meltLog10(metadata = metadata) %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~description,
                          y = ~counts,
                          color = ~group)
        ) +
        ggplot2::ggtitle(paste("counts per gene:", countsName)) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::xlab("sample") +
        ggplot2::ylab(expression(log[10]~counts~per~gene))
    if (isTRUE(flip)) {
        plot <- plot +
            ggplot2::coord_flip()
    }
    if (isTRUE(print)) {
        print(plot)
    } else {
        return(plot)
    }
}
