#' Count density plot
#'
#' @author Michael Steinbaugh
#'
#' @import ggplot2
#'
#' @param counts Counts matrix
#' @param metadata Metadata data frame
#' @param print Print plot
#'
#' @return Density plot
#' @export
countDensity <- function(counts,
                         metadata,
                         print = TRUE) {
    countsName <- deparse(substitute(counts))
    plot <- counts %>%
        meltLog10(metadata = metadata) %>%
        ggplot2::ggplot(
            ggplot2::aes_(x = ~counts,
                          group = ~description)
        ) +
        ggplot2::ggtitle(paste("count density:", countsName)) +
        ggplot2::geom_density() +
        ggplot2::xlab(expression(log[10]~counts~per~gene)) +
        ggplot2::ylab("density")
    if (isTRUE(print)) {
        print(plot)
    } else {
        return(plot)
    }
}
