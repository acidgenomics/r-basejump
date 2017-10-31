#' Prepare Annotable
#'
#' @rdname prepareAnnotable
#' @name prepareAnnotable
#'
#' @inheritParams AllGenerics
#'
#' @description
#' Standardize a user-defined annotable:
#'
#' 1. Define `broadClass` column, based on `biotype` column.
#' 2. Ensure rownames are set to Ensembl gene identifier.
#'
#' This gets used automatically by the [annotable()] function, but is also
#' useful for standardizing a user-defined annotable, such as the output
#' from Stephen Turner's annotables package. The annotables package has support
#' for Homo sapiens GRCh37/hg19, which currently isn't supported in
#' AnnotationHub.
#'
#' @return [data.frame].
NULL



# Constructors ====
#' @importFrom dplyr case_when mutate
#' @importFrom magrittr set_rownames
.prepareAnnotable <- function(object) {
    object %>%
        # Improve handling of `NA` uniques here
        fixNA() %>%
        mutate(
            # Ensure unique symbols (e.g. human, mouse)
            symbol = make.unique(.data[["symbol"]]),
            # Define the broad class
            broadClass = case_when(
                grepl(
                    x = .data[["symbol"]],
                    # Hsapiens: `MT-`,
                    # Mmusculus: `mt-`
                    # Dmelanogaster: `mt:`
                    pattern = "^mt[\\:\\-]",
                    ignore.case = TRUE) ~ "mito",
                .data[["biotype"]] == "protein_coding" ~ "coding",
                .data[["biotype"]] %in%
                    c("known_ncrna",
                      "lincRNA",
                      "non_coding") ~ "noncoding",
                grepl(
                    x = .data[["biotype"]],
                    pattern = "pseudo") ~ "pseudo",
                .data[["biotype"]] %in%
                    c("miRNA",
                      "misc_RNA",
                      "ribozyme",
                      "rRNA",
                      "scaRNA",
                      "scRNA",
                      "snoRNA",
                      "snRNA",
                      "sRNA") ~ "small",
                .data[["biotype"]] %in%
                    c("non_stop_decay",
                      "nonsense_mediated_decay") ~ "decaying",
                grepl(
                    x = .data[["biotype"]],
                    pattern = "^ig_",
                    ignore.case = TRUE) ~ "ig",
                grepl(
                    x = .data[["biotype"]],
                    pattern = "^tr_",
                    ignore.case = TRUE) ~ "tcr",
                TRUE ~ "other")) %>%
        set_rownames(.[["ensgene"]])
}



# Methods ====
#' @rdname prepareAnnotable
#' @export
setMethod(
    "prepareAnnotable",
    signature("data.frame"),
    .prepareAnnotable)
