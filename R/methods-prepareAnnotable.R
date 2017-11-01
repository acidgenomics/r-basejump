#' Prepare Annotable
#'
#' @rdname prepareAnnotable
#' @name prepareAnnotable
#' @author Broad class definitions by Rory Kirchner
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
#' @inheritParams AllGenerics
#'
#' @param dropExtraCols Drop extra columns.
#'
#' @return [data.frame].
NULL



# Constructors ====
#' @importFrom dplyr case_when distinct group_by mutate ungroup
#' @importFrom magrittr set_rownames
#' @importFrom rlang !!! syms
#' @importFrom tidyr nest
.prepareAnnotable <- function(object, dropExtraCols = TRUE) {
    # Check for required columns
    requiredCols <- c("ensgene", "symbol", "description" , "biotype")
    if (!all(requiredCols %in% colnames(object))) {
        stop(paste(
            "Required columns:",
            toString(requiredCols)
        ), call. = FALSE)
    }

    # Handle non-standard extra columns
    if (isTRUE(dropExtraCols)) {
        object <- object[, requiredCols] %>%
            distinct()
    } else {
        # Attempt to make distinct by nesting if ensgene isn't unique
        if (any(duplicated(object[["ensgene"]]))) {
            object <- object %>%
                group_by(!!!syms(requiredCols)) %>%
                nest(.key = "nestedData") %>%
                ungroup()
        }
    }

    # Now ensure that Ensembl identifiers are unique
    if (any(duplicated(object[["ensgene"]]))) {
        stop("Duplicate Ensembl identifiers detected", call. = FALSE)
    }

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
        as.data.frame() %>%
        set_rownames(.[["ensgene"]])
}



# Methods ====
#' @rdname prepareAnnotable
#' @export
setMethod(
    "prepareAnnotable",
    signature("data.frame"),
    .prepareAnnotable)
