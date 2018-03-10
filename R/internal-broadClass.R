.addBroadClassCol <- function(object) {
    assert_is_any_of(object, ensemblReturn)
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }

    # Biotype (genes or transcripts)
    biotypeCol <- grep(
        pattern = "biotype",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    assert_is_a_string(biotypeCol)
    biotype <- data[[biotypeCol]]

    # Symbol (not present for transcripts)
    assert_is_subset("symbol", colnames(data), severity = "warning")
    symbol <- data[["symbol"]]

    # Seqname (chromosome)
    assert_is_subset("seqName", colnames(data), severity = "warning")
    seqName <- data[["seqName"]]

    tibble <- tibble(
        symbol = symbol,
        biotype = biotype,
        seqName = seqName
    )

    data[["broadClass"]] <- .broadClass(tibble)

    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }

    object
}



#' Broad Class Definitions
#'
#' @author Rory Kirchner and Michael Steinbaugh
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @param param Gene or transcript biotype.
#' @param symbol *Optional*. Gene symbol.
#'
#' @return Named character vector containing broad class definitions.
.broadClass <- function(object) {
    stopifnot(identical(
        x = colnames(object),
        y = c("symbol", "biotype", "seqName")
    ))
    case_when(
        object[["seqName"]] == "MT" ~ "mito",
        grepl(
            # Hsapiens: "MT-*",
            # Mmusculus: "mt-*"
            # Dmelanogaster: "mt:*"
            pattern = "^mt[\\:\\-]",
            x = object[["symbol"]],
            ignore.case = TRUE
        ) ~ "mito",
        object[["biotype"]] == "protein_coding" ~ "coding",
        object[["biotype"]] %in% c(
            "known_ncrna",
            "lincRNA",
            "non_coding"
        ) ~ "noncoding",
        grepl(
            pattern = "pseudo",
            x = object[["biotype"]]
        ) ~ "pseudo",
        object[["biotype"]] %in% c(
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
        object[["biotype"]] %in% c(
            "non_stop_decay",
            "nonsense_mediated_decay"
        ) ~ "decaying",
        grepl(
            pattern = "^ig_",
            x = object[["biotype"]],
            ignore.case = TRUE
        ) ~ "ig",
        grepl(
            pattern = "^tr_",
            x = object[["biotype"]],
            ignore.case = TRUE
        ) ~ "tcr",
        TRUE ~ "other"
    )
}
