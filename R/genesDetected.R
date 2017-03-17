#' Genes detected plot
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import ggplot2
#'
#' @param summary \code{bcbio-rnaseq} summary report
#'
#' @export
#'
#' @examples
#' \dontrun{
#' genesDetected(summary)
#' }
genesDetected <- function(summary) {
    summary %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~description,
                          y = ~colSums(raw_counts > 0),
                          fill = ~group)
        ) +
        ggplot2::ggtitle("Number of genes detected") +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_hline(color = "green",
                            size = 2,
                            yintercept = 20000) +
        ggplot2::xlab("sample") +
        ggplot2::ylab("gene count") +
        ggplot2::coord_flip()
}
