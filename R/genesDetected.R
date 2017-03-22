#' Genes detected plot
#'
#' @author Michael Steinbaugh
#'
#' @import ggplot2
#'
#' @param counts Counts matrix
#' @param summary \code{bcbio-rnaseq} summary report
#'
#' @export
#'
#' @examples
#' \dontrun{
#' genesDetected(summary)
#' }
genesDetected <- function(counts, summary) {
    countSums <- colSums(counts > 0)
    summary %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~description,
                          y = countSums,
                          fill = ~group)
        ) +
        ggplot2::ggtitle("genes detected") +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_hline(color = "green",
                            size = 2,
                            yintercept = 20000) +
        ggplot2::xlab("sample") +
        ggplot2::ylab("gene count") +
        ggplot2::coord_flip()
}
