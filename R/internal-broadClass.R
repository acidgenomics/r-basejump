# Default to snake case, to match unmodified ensembldb output
.addBroadClassCol <- function(object, makeNames = "snake") {
    assert_is_any_of(object, c("data.frame", "DataFrame", "GRanges"))
    makeNames <- .getMakeNamesFunction(makeNames)

    # Metadata
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }
    assert_has_colnames(data)

    # Biotype
    assert_any_are_matching_regex(colnames(data), "biotype")
    biotypeCol <- grep("biotype", colnames(data), value = TRUE)
    assert_is_a_string(biotypeCol)
    biotype <- data[, biotypeCol, drop = TRUE]

    # Symbol
    if ("symbol" %in% colnames(data)) {
        symbol <- data[, "symbol", drop = TRUE]
    } else {
        symbol <- NA
    }

    # Gene/transcript ID (rownames)
    idCol <- biotypeCol %>%
        gsub("_biotype$", "", .) %>%
        paste0(., "_id")
    assert_is_subset(idCol, colnames(data))
    rownames <- data[, idCol, drop = TRUE]

    df <- data.frame(
        biotype = biotype,
        symbol = symbol,
        row.names = rownames,
        stringsAsFactors = FALSE)
    broad <- .broadClass(df)

    if (is(object, "GRanges")) {
        mcols(object)[["broad_class"]] <- broad
    } else {
        object[["broad_class"]] <- broad
    }

    makeNames(object)
}



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
#' @return Named character vector containing broad class definitions.
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
        pull("broad")
    names(broad) <- rownames(object)
    broad
}
