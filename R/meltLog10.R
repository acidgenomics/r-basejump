#' Melt RNA-Seq count data to long format and log10 transform
#' @export
#' @importFrom dplyr mutate_
#' @importFrom reshape2 melt
#' @importFrom stats setNames
#' @param rawCounts Raw counts `matrix`
#' @param metadata Metadata `data.frame`
#' @return log10 melted `data.frame`
meltLog10 <- function(rawCounts, metadata = NULL) {
    meltedCounts <- rawCounts %>%
        as.data.frame %>%
        reshape2::melt(.,
                       variable.name = "description",
                       value.name = "counts") %>%
        subset(counts > 0)
    if (!is.null(metadata)) {
        meltedCounts <- merge(meltedCounts, metadata)
    }
    meltedCounts %>%
        dplyr::mutate_(.dots = stats::setNames(list(quote(log(counts))), "counts"))
}

# Work on suppressing this warning message:
# No id variables; using all as measure variables
