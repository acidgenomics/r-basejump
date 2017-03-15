#' Total reads plot
#'
#' Optimized for \code{bcbio-rnaseq} summary report.
#'
#' @author Michael Steinbaugh
#' @keywords bcbio plot rnaseq
#'
#' @import ggplot2
#'
#' @param description Sample description
#' @param totalReads Total number of reads
#' @param group Sample grouping, used for coloring
#'
#' @export
#'
#' @examples
#' \dontrun{
#' totalReads(summary$description,
#'            summary$total_reads,
#'            summary$treatment)
#' }
totalReads <- function(description,
                       totalReads,
                       group) {
    data.frame(description = description,
               group = group,
               totalReads = totalReads / 1e6) %>%
        ggplot2::ggplot(
            aes_(x = ~description,
                 y = ~totalReads,
                 fill = ~group)
        ) +
        ggplot2::ggtitle("Total reads") +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_hline(linetype = 2,
                            yintercept = 20) +
        ggplot2::xlab("sample") +
        ggplot2::ylab("total reads (million)") +
        ggplot2::coord_flip()
}
