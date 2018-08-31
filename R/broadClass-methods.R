#' Broad Class Definitions
#'
#' @name broadClass
#' @family Annotation Functions
#' @author Rory Kirchner and Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Object that can be coerced to `DataFrame`, containing gene or
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



#' @rdname broadClass
#' @export
setMethod(
    "broadClass",
    signature("GRanges"),
    function(object) {
        data <- as(object, "tbl_df")
        assert_are_identical(data[["rowname"]], names(object))
        rownames <- data[["rowname"]]

        # Early return if already defined
        if ("broadClass" %in% colnames(data)) {
            broad <- data[["broadClass"]]
            names(broad) <- rownames
            return(broad)
        }

        # geneName (required)
        assert_is_subset("geneName", colnames(data))
        geneName <- data[["geneName"]]

        # Biotype (optional)
        # Prioritize transcript over gene, if present
        biotypeCol <- grep(
            pattern = "biotype$",
            x = colnames(data),
            ignore.case = TRUE,
            value = TRUE
        )
        if (length(biotypeCol)) {
            biotypeCol <- biotypeCol[[1L]]
            biotype <- data[[biotypeCol]]
        } else {
            # nocov start
            warning("biotype missing", call. = FALSE)
            biotype <- NA
            # nocov end
        }

        # seqname (optional; aka chromosome)
        seqnameCol <- grep(
            pattern = "seqname",
            x = colnames(data),
            ignore.case = TRUE,
            value = TRUE
        )
        if (length(seqnameCol)) {
            seqnameCol <- seqnameCol[[1L]]
            seqname <- data[[seqnameCol]]
        } else {
            # nocov start
            warning("seqname missing", call. = FALSE)
            seqname <- NA
            # nocov end
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
        names(broad) <- rownames
        broad
    }
)



#' @rdname broadClass
#' @export
setMethod(
    "broadClass",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        rowData(object)[["broadClass"]]
    }
)
