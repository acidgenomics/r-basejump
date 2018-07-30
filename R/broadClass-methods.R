#' Broad Class Definitions
#'
#' @name broadClass
#' @family Gene Annotation Functions
#' @author Rory Kirchner and Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Object that can be coerced to `data.frame`, containing gene or
#'   transcript annotations.
#'
#' @return Named `factor` containing broad class definitions.
#'
#' @examples
#' # GRanges ====
#' x <- broadClass(makeGRangesFromEnsembl("Homo sapiens"))
#' table(x)
#'
#' # SummarizedExperiment ====
#' x <- broadClass(rse_bcb)
#' table(x)
NULL



# Methods ======================================================================
#' @rdname broadClass
#' @export
setMethod(
    "broadClass",
    signature("GRanges"),
    function(object) {
        object <- as.data.frame(object)
        assertHasRownames(object)

        # Early return if already defined
        if ("broadClass" %in% colnames(object)) {
            broad <- object[["broadClass"]]
            names(broad) <- rownames(object)
            return(broad)
        }

        # geneName (required)
        assert_is_subset("geneName", colnames(object))
        geneName <- object[["geneName"]]

        # Biotype (optional)
        # Prioritize transcript over gene, if present
        biotypeCol <- grep(
            pattern = "biotype$",
            x = colnames(object),
            ignore.case = TRUE,
            value = TRUE
        )
        if (length(biotypeCol)) {
            biotypeCol <- biotypeCol[[1L]]
            biotype <- object[[biotypeCol]]
        } else {
            warning("biotype missing", call. = FALSE)
            biotype <- NA
        }

        # seqname (optional; aka chromosome)
        seqnameCol <- grep(
            pattern = "seqname",
            x = colnames(object),
            ignore.case = TRUE,
            value = TRUE
        )
        if (length(seqnameCol)) {
            seqnameCol <- seqnameCol[[1L]]
            seqname <- object[[seqnameCol]]
        } else {
            warning("seqname missing", call. = FALSE)
            seqname <- NA
        }

        message(paste(
            "Defining broad class using:",
            toString(c("geneName", biotypeCol, seqnameCol))
        ))

        data <- tibble(
            geneName = geneName,
            biotype = biotype,
            seqname = seqname
        )

        broad <- case_when(
            data[["seqname"]] == "MT" ~ "mito",
            grepl(
                pattern = "^mt[\\:\\-]",
                x = data[["geneName"]],
                ignore.case = TRUE
            ) ~ "mito",
            data[["biotype"]] == "protein_coding" ~ "coding",
            data[["biotype"]] %in% c(
                "known_ncrna",
                "lincRNA",
                "non_coding"
            ) ~ "noncoding",
            grepl(
                pattern = "pseudo",
                x = data[["biotype"]]
            ) ~ "pseudo",
            data[["biotype"]] %in% c(
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
            data[["biotype"]] %in% c(
                "non_stop_decay",
                "nonsense_mediated_decay"
            ) ~ "decaying",
            grepl(
                pattern = "^ig_",
                x = data[["biotype"]],
                ignore.case = TRUE
            ) ~ "ig",
            grepl(
                pattern = "^tr_",
                x = data[["biotype"]],
                ignore.case = TRUE
            ) ~ "tcr",
            TRUE ~ "other"
        )

        broad <- as.factor(broad)
        names(broad) <- rownames(object)
        broad
    }
)



#' @rdname broadClass
#' @export
setMethod(
    "broadClass",
    signature("SummarizedExperiment"),
    function(object) {
        rowData(object)[["broadClass"]]
    }
)
