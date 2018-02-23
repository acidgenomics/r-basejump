#' Broad Class Definitions
#'
#' @author Rory Kirchner and Michael Steinbaugh
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr case_when mutate pull
#' @importFrom rlang .data
#'
#' @param param Gene or transcript biotype.
#' @param symbol *Optional*. Gene symbol.
#'
#' @return Named factor containing broad class definitions.
.broadClass <- function(object) {
    assert_is_data.frame(object)
    assertHasRownames(object)
    assert_is_subset(c("biotype", "symbol"), colnames(object))
    broad <- object %>%
        mutate(
            broad = case_when(
                grepl(
                    x = .data[["symbol"]],
                    # Hsapiens: `MT-`,
                    # Mmusculus: `mt-`
                    # Dmelanogaster: `mt:`
                    pattern = "^mt[\\:\\-]",
                    ignore.case = TRUE
                ) ~ "mito",
                .data[["biotype"]] == "protein_coding" ~ "coding",
                .data[["biotype"]] %in% c(
                    "known_ncrna",
                    "lincRNA",
                    "non_coding"
                ) ~ "noncoding",
                grepl(
                    pattern = "pseudo",
                    x = .data[["biotype"]]
                ) ~ "pseudo",
                .data[["biotype"]] %in% c(
                    "miRNA",
                    "misc_RNA",
                    "ribozyme",
                    "rRNA",
                    "scaRNA",
                    "scRNA",
                    "snoRNA",
                    "snRNA",
                    "sRNA"
                ) ~ "small",
                .data[["biotype"]] %in% c(
                    "non_stop_decay",
                    "nonsense_mediated_decay"
                ) ~ "decaying",
                grepl(
                    pattern = "^ig_",
                    x = .data[["biotype"]],
                    ignore.case = TRUE
                ) ~ "ig",
                grepl(
                    pattern = "^tr_",
                    x = .data[["biotype"]],
                    ignore.case = TRUE
                ) ~ "tcr",
                TRUE ~ "other")
        ) %>%
        pull("broad") %>%
        as.factor()
    names(broad) <- rownames(object)
    broad
}
