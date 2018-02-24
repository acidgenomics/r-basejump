#' Define Transcript to Gene Mappings from GFF/GTF File
#'
#' @rdname tx2geneFromGFF
#' @name tx2geneFromGFF
#' @family Gene Annotation Utilities
#'
#' @inherit tx2gene
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
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
    })



#' @rdname tx2geneFromGFF
#' @importFrom dplyr arrange distinct
#' @importFrom rlang !! sym
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

        enstxp <- str_match(anno, "transcript_id ([^;]+);") %>%
            .[, 2L]
        ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
            .[, 2L]

        assert_all_are_non_missing_nor_empty_character(enstxp)
        assert_all_are_non_missing_nor_empty_character(ensgene)
        assert_are_same_length(enstxp, ensgene)

        data <- cbind(enstxp, ensgene) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            distinct() %>%
            arrange(!!sym("enstxp")) %>%
            set_rownames(.[["enstxp"]])

        # Check that all transcripts are unique
        assert_has_no_duplicates(data[["enstxp"]])

        inform(paste(
            "tx2gene mappings:",
            length(unique(data[["enstxp"]])), "transcripts,",
            length(unique(data[["ensgene"]])), "genes"
        ))

        data
    })



# Aliases ======================================================================
#' @rdname tx2geneFromGFF
#' @inheritParams general
#' @export
tx2geneFromGTF <- function(...) {
    tx2geneFromGFF(...)
}
