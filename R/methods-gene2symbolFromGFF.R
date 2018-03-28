#' Define Gene to Symbol Mappings from GFF/GTF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @name gene2symbolFromGFF
#' @family Gene Functions
#'
#' @inheritParams general
#'
#' @return `data.frame`.
#'
#' @examples
#' # character ====
#' # From URL (recommended)
#' url <- "http://basejump.seq.cloud/mmusculus.gtf"
#' gene2symbolFromGFF(url) %>% glimpse()
#'
#' # data.frame ====
#' gff <- readGFF(url)
#' gene2symbolFromGFF(gff) %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("character"),
    function(object) {
        object %>%
            readGFF() %>%
            gene2symbolFromGFF()
    }
)



#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("data.frame"),
    function(object) {
        assertIsGFF(object)
        data <- .gffKeyValuePairs(object, unique = TRUE)

        # Standardize `geneName` column (Ensembl format).
        # This fix is necessary for FlyBase GFF files.
        if ("geneSymbol" %in% colnames(data)) {
            data[["geneName"]] <- data[["geneSymbol"]]
        }

        data <- data[, c("geneID", "geneName")] %>%
            mutate_if(is.factor, as.character) %>%
            .[!is.na(.[["geneID"]]), , drop = FALSE] %>%
            .[!is.na(.[["geneName"]]), , drop = FALSE] %>%
            unique() %>%
            .[order(.[["geneID"]]), , drop = FALSE] %>%
            set_rownames(.[["geneID"]])
        assert_has_no_duplicates(data[["geneID"]])

        inform(paste(
            "gene2symbol mappings:",
            length(unique(data[["geneID"]])), "genes"
        ))

        data
    }
)



# Aliases ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
gene2symbolFromGTF <- function(...) {
    gene2symbolFromGFF(...)  # nocov
}
