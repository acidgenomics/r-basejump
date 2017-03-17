#' Melt RNA-Seq count data to long format and log10 transform
#'
#' @author Michael Steinbaugh
#' @keywords bcbio rnaseq
#'
#' @import dplyr
#' @import reshape2
#' @import tibble
#'
#' @param rawCounts Raw counts matrix
#' @param metadata Metadata data frame
#'
#' @return log10 melted data frame
#' @export
meltLog10 <- function(rawCounts, metadata) {
    rawCounts %>%
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

# variable.name = "description",
# value.name = "counts"

# Work on suppressing this warning message:
# No id variables; using all as measure variables
