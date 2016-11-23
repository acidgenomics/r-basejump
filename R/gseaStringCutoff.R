#' Reformat STRINGdb to a tibble with significance cutoffs
#' 
#' @import dplyr
#' @import magrittr
#' @import tibble
#'
#' @param gseaString STRINGdb list from `gseaString()`.
#' @param count Minimum count of hits.
#' @param p P value cutoff value.
#' @param fdr False discovery rate cutoff value.
#'
#' @return Tibble or kable depending on the call.
#' @export
gseaStringCutoff <- function(gseaString,
                             count = getOption("count"),
                             p = getOption("p"),
                             fdr = getOption("fdr")) {
    gseaString$id <- NULL
    gseaString$backgroundV <- NULL
    list <- lapply(gseaString, function(tibble) {
        tibble <- tibble %>%
            tibble::as_tibble(.) %>%
            magrittr::set_names(camel(names(.))) %>%
            dplyr::select_(.dots = c("termId",
                                     "termDescription",
                                     "hits",
                                     "pvalue",
                                     "pvalueFdr")) %>%
            dplyr::rename_(.dots = c("identifier" = "termId",
                                     "description" = "termDescription",
                                     "count" = "hits",
                                     "p" = "pvalue",
                                     "fdr" = "pvalueFdr")) %>%
            dplyr::arrange_(.dots = c("p"))
        if (!is.null(count)) {
            tibble <- tibble[tibble$count >= count, ]
        }
        if (!is.null(p)) {
            tibble <- tibble[tibble$p < p, ]
        }
        if (!is.null(fdr)) {
            tibble <- tibble[tibble$fdr < fdr, ]
        }
    })
    return(list)
}
