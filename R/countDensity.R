#' Count density plot
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import ggplot2
#'
#' @param counts Counts matrix
#' @param metadata Metadata data frame
#'
#' @return Density plot
#' @export
countDensity <- function(counts, metadata) {
    countsName <- deparse(substitute(counts))
    counts %>%
        meltLog10(metadata = metadata) %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~counts,
                          group = ~description)
        ) +
        ggplot2::ggtitle(paste("Count density:", countsName)) +
        ggplot2::geom_density() +
        ggplot2::xlab(expression(log[10]~counts~per~gene)) +
        ggplot2::ylab("density")
}
