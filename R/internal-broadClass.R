.addBroadClassCol <- function(object) {
    assert_is_any_of(object, ensemblReturn)
    # Note that DataFrame class nests the GRanges containing seqnames. When we
    # coerce to data.frame here, it gets coerced to `x.*` columns.
    data <- as.data.frame(object)
    if (is(object, "GRanges")) {
        rownames(data) <- names(object)
    }
    assertHasRownames(data)

    # geneName (aka symbol)
    assert_is_subset("geneName", colnames(data))
    geneName <- data[["geneName"]]

    # Biotype (prioritize transcript over gene, if present)
    biotypeCol <- grep(
        pattern = "biotype$",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    assert_is_non_empty(biotypeCol)
    biotypeCol <- biotypeCol[[1L]]
    biotype <- data[[biotypeCol]]

    # seqnames (aka chromosome)
    # FIXME This doesn't get returned for transcript data frames by ensembldb
    seqnamesCol <- grep(
        pattern = "seqname",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    assert_is_non_empty(seqnamesCol, severity = "warning")
    if (length(seqnamesCol)) {
        seqnamesCol <- seqnamesCol[[1L]]
        seqnames <- data[[seqnamesCol]]
    } else {
        seqnames <- NA
    }

    inform(paste(
        "Defining broadClass using:",
        toString(c("geneName", biotypeCol, "seqnames"))
    ))

    broadClass <- data.frame(
        "geneName" = geneName,
        "biotype" = biotype,
        "seqnames" = seqnames,
        row.names = rownames(data)
    ) %>%
        .broadClass()

    if (is(object, "GRanges")) {
        mcols(object)[["broadClass"]] <- broadClass
    } else {
        object[["broadClass"]] <- broadClass
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
#'
#' @return Named `factor` containing broad class definitions.
.broadClass <- function(object) {
    assertHasRownames(object)
    assert_is_subset(
        x = c("geneName", "biotype", "seqnames"),
        y = colnames(object)
    )
    broad <- case_when(
        object[["seqnames"]] == "MT" ~ "mito",
        grepl(
            # Hsapiens: "MT-*",
            # Mmusculus: "mt-*"
            # Dmelanogaster: "mt:*"
            pattern = "^mt[\\:\\-]",
            x = object[["geneName"]],
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
    broad <- as.factor(broad)
    names(broad) <- rownames(object)
    broad
}
