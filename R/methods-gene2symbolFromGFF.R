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
#' # From URL (recommended)
#' url <- "http://basejump.seq.cloud/mmusculus.gtf"
#' gene2symbolFromGFF(url) %>% glimpse()
#'
#' # GFF data.frame
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
#' @importFrom stringr str_match
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("data.frame"),
    function(object) {
        assert_is_data.frame(object)
        assert_are_identical(ncol(object), 9L)

        anno <- .gffKeyValuePairs(object)
        assert_is_character(anno)

        # Standard `gene_symbol` to `gene_name` (Ensembl format).
        # This fix is necessary for FlyBase GFF files.
        if (any(grepl("gene_symbol", anno))) {
            anno <- gsub("gene_symbol", "gene_name", anno)
        }

        anno <- anno %>%
            .[grepl("gene_id", .)] %>%
            .[grepl("gene_name", .)] %>%
            unique()

        geneID <- str_match(anno, "gene_id ([^;]+);") %>%
            .[, 2L]
        assert_all_are_non_missing_nor_empty_character(geneID)
        geneName <- str_match(anno, "gene_name ([^;]+);") %>%
            .[, 2L]
        assert_all_are_non_missing_nor_empty_character(geneName)
        assert_are_same_length(geneID, geneName)
        data <- cbind(geneID, geneName) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            unique() %>%
            .[order(.[["geneID"]]), , drop = FALSE]

        # Check that all transcripts are unique
        assert_has_no_duplicates(data[["geneID"]])

        inform(paste(
            "gene2symbol mappings:",
            length(unique(data[["geneID"]])), "genes"
        ))

        rownames(data) <- data[["geneID"]]
        data
    }
)



# Aliases ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
gene2symbolFromGTF <- function(...) {
    gene2symbolFromGFF(...)  # nocov
}
