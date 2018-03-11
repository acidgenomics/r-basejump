#' Define Transcript to Gene Mappings from GFF/GTF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @name tx2geneFromGFF
#' @family Gene Functions
#'
#' @inherit tx2gene
#'
#' @examples
#' # From URL (recommended)
#' url <- "http://basejump.seq.cloud/mmusculus.gtf"
#' tx2geneFromGFF(url) %>% glimpse()
#'
#' # GFF data.frame
#' gff <- readGFF(url)
#' tx2geneFromGFF(gff) %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("character"),
    function(object) {
        object %>%
            readGFF() %>%
            tx2geneFromGFF()
    }
)



#' @rdname tx2geneFromGFF
#' @importFrom magrittr set_rownames
#' @importFrom stringr str_match
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("data.frame"),
    function(object) {
        assert_is_data.frame(object)
        assert_are_identical(ncol(object), 9L)

        anno <- object %>%
            .gffKeyValuePairs() %>%
            .[grepl("transcript_id", .)] %>%
            .[grepl("gene_id", .)] %>%
            unique()

        txID <- str_match(anno, "transcript_id ([^;]+);") %>%
            .[, 2L]
        assert_all_are_non_missing_nor_empty_character(txID)
        geneID <- str_match(anno, "gene_id ([^;]+);") %>%
            .[, 2L]
        assert_all_are_non_missing_nor_empty_character(geneID)
        assert_are_same_length(txID, geneID)
        data <- cbind(txID, geneID) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            unique() %>%
            .[order(.[["txID"]]), , drop = FALSE] %>%
            set_rownames(.[["txID"]])

        # Check that all transcripts are unique
        assert_has_no_duplicates(data[["txID"]])

        inform(paste(
            "tx2gene mappings:",
            length(unique(data[["txID"]])), "transcripts,",
            length(unique(data[["geneID"]])), "genes"
        ))

        data
    }
)



# Aliases ======================================================================
#' @rdname tx2geneFromGFF
#' @export
tx2geneFromGTF <- function(...) {
    tx2geneFromGFF(...)  # nocov
}
