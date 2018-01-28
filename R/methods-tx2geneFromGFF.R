#' Define Transcript to Gene Mappings from GFF/GTF File
#'
#' @rdname tx2geneFromGFF
#' @name tx2geneFromGFF
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @return [data.frame].
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



# Constructors =================================================================
.gffKeyValuePairs <- function(object) {
    object %>%
        .[[9L]] %>%
        unique()
}



#' @importFrom dplyr arrange distinct
#' @importFrom magrittr set_rownames
#' @importFrom stringr str_match
.tx2geneFromGFF <- function(
    object,
    quiet = FALSE) {
    anno <- object %>%
        .gffKeyValuePairs() %>%
        .[grepl("transcript_id", .) && grepl("gene_id", .)] %>%
        unique()

    enstxp <- str_match(anno, "transcript_id ([^;]+);") %>%
        .[, 2L]
    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]

    df <- cbind(enstxp, ensgene) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        distinct() %>%
        arrange(!!sym("enstxp")) %>%
        set_rownames(.[["enstxp"]])

    if (!isTRUE(quiet)) {
        inform(paste(
            "tx2gene mappings:",
            nrow(df), "transcripts,",
            length(unique(df[["ensgene"]])), "genes"))
    }

    df
}



# Methods ======================================================================
#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        object %>%
            readGFF(quiet = quiet) %>%
            .tx2geneFromGFF(quiet = quiet)
    })



#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("data.frame"),
    function(
        object,
        quiet = FALSE) {
        if (dim(object)[[2L]] != 9L) {
            abort("GFF object must be data.frame with 9 columns")
        }
        .tx2geneFromGFF(object, quiet = quiet)
    })
