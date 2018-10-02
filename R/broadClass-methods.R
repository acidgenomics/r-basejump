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
#' # SummarizedExperiment ====
#' x <- broadClass(rse_small)
#' table(x)
NULL



.broadClass.GRanges <-  # nolint
    function(object) {
        data <- as(object, "tbl_df")
        assert_are_identical(data[["rowname"]], names(object))
        rownames <- data[["rowname"]]

        # Early return if already defined.
        if ("broadClass" %in% colnames(data)) {
            broad <- data[["broadClass"]]
            names(broad) <- rownames
            return(broad)
        }

        # Gene name (required).
        assert_is_subset("geneName", colnames(data))
        geneName <- data[["geneName"]]

        # Biotype (optional)
        # Prioritize transcript over gene, if present.
        biotypeCol <- grep(
            pattern = "biotype$",
            x = colnames(data),
            ignore.case = TRUE,
            value = TRUE
        )
        if (has_length(biotypeCol)) {
            biotypeCol <- biotypeCol[[1L]]
            biotype <- data[[biotypeCol]]
        } else {
            # nocov start
            warning("Biotype column is missing.", call. = FALSE)
            biotype <- NA
            # nocov end
        }

        # Seqname (optional; aka chromosome).
        seqnameCol <- grep(
            pattern = "seqname",
            x = colnames(data),
            ignore.case = TRUE,
            value = TRUE
        )
        if (has_length(seqnameCol)) {
            seqnameCol <- seqnameCol[[1L]]
            seqname <- data[[seqnameCol]]
        } else {
            # nocov start
            warning("`seqname` column is missing.", call. = FALSE)
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
            # Consider using `NA_character_` here instead.
            TRUE ~ "other"
        )

        broad <- as.factor(broad)
        names(broad) <- rownames
        broad
    }



.broadClass.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- rowData(object)[["broadClass"]]
        if (is.character(data)) {
            data <- as.factor(data)
        }
        assert_is_factor(data)
        data
    }



#' @rdname broadClass
#' @export
setMethod(
    f = "broadClass",
    signature = signature("GRanges"),
    definition = .broadClass.GRanges
)



#' @rdname broadClass
#' @export
setMethod(
    f = "broadClass",
    signature = signature("SummarizedExperiment"),
    definition = .broadClass.SE
)
