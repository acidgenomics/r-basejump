#' Generate tx2gene from GFF/GTF File
#'
#' @rdname tx2geneFromGFF
#' @name tx2geneFromGFF
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @return [data.frame].
#'
#' @examples
#' # From URL (recommended)
#' url <- file.path(testDataURL, "mmusculus.gtf")
#' tx2geneFromGFF(url) %>%
#'     str()
#'
#' # GFF data.frame
#' gff <- readGFF(url)
#' tx2geneFromGFF(gff) %>%
#'     str()
NULL



# Constructors ====
.gffKeyValuePairs <- function(object) {
    object %>%
        .[[9L]] %>%
        unique()
}



.tx2geneFromGFF <- function(object) {
    anno <- object %>%
        .gffKeyValuePairs() %>%
        .[str_detect(., "transcript_id") & str_detect(., "gene_id")] %>%
        unique()

    enstxp <- str_match(anno, "transcript_id ([^;]+);") %>%
        .[, 2L]
    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]

    df <- cbind(enstxp, ensgene) %>%
        as.data.frame() %>%
        distinct() %>%
        arrange(!!sym("enstxp")) %>%
        set_rownames(.[["enstxp"]])

    message(paste(
        "tx2gene mappings:",
        nrow(df), "transcripts,",
        length(unique(df[["ensgene"]])), "genes"))

    df
}



# Methods ====
#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("character"),
    function(object) {
        object %>%
            readGFF() %>%
            .tx2geneFromGFF()
    })



#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("data.frame"),
    function(object) {
        if (dim(object)[[2L]] != 9L) {
            stop("GFF object must be data.frame with 9 columns",
                 call. = FALSE)
        }
        .tx2geneFromGFF(object)
    })
