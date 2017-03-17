#' Mapped reads plot
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
#' totalReads(summary)
#' }
mappedReads <- function(summary) {
    summary %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~description,
                          y = ~mapped_reads / 1e6,
                          fill = ~group)
        ) +
        ggplot2::ggtitle("Mapped reads") +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_hline(color = "orange",
                            size = 2,
                            yintercept = 10) +
        ggplot2::geom_hline(color = "green",
                            size = 2,
                            yintercept = 20) +
        ggplot2::xlab("sample") +
        ggplot2::ylab("mapped reads (million)") +
        ggplot2::coord_flip()
}
