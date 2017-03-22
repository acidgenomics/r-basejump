#' Melt RNA-Seq count data to long format and log10 transform
#'
#' @author Michael Steinbaugh
#'
#' @import dplyr
#' @import reshape2
#' @import tibble
#'
#' @param counts Counts matrix
#' @param metadata Metadata data frame
#'
#' @return log10 melted data frame
#' @export
meltLog10 <- function(counts, metadata) {
    counts %>%
        as.data.frame %>%
        tibble::rownames_to_column(.) %>%
        reshape2::melt(., id = 1) %>%
        setNames(c("ensembl_gene",
                   "description",
                   "counts")) %>%
        subset(counts > 0) %>%
        merge(metadata) %>%
        dplyr::mutate_(.dots = setNames(list(quote(log(counts))), "counts"))
}
