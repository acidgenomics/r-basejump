#' Reformat RDAVIDWebService functionalAnnotationChart to a tibble with significance cutoffs
#' 
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @importFrom stats setNames
#'
#' @param gseaDavid RDAVIDWebService list from `gseaDavid()`.
#' @param count Minimum count of hits.
#' @param p P value cutoff value.
#' @param fdr False discovery rate cutoff value.
#'
#' @return Tibble or kable depending on the call.
#' @export
gseaDavidCutoff <- function(gseaDavid,
                            count = getOption("count"),
                            p = getOption("p"),
                            fdr = getOption("fdr")) {
    tibble <- gseaDavid[["functionalAnnotationChart"]] %>%
        data.frame(.) %>%
        tibble::as_tibble(.) %>%
        magrittr::set_names(camel(names(.))) %>%
        dplyr::select_(.dots = c("category",
                                 "term",
                                 "count",
                                 "pvalue",
                                 "fdr")) %>%
        dplyr::rename_(.dots = c("p" = "pvalue")) %>%
        dplyr::mutate_(.dots = stats::setNames(list(~fdr / 100), "fdr")) %>%
        dplyr::arrange_(.dots = c("category", "fdr"))
    if (!is.null(count)) {
        tibble <- tibble[tibble$count >= count, ]
    }
    if (!is.null(p)) {
        tibble <- tibble[tibble$p < p, ]
    }
    if (!is.null(fdr)) {
        tibble <- tibble[tibble$fdr < fdr, ]
    }
    return(tibble)
}
