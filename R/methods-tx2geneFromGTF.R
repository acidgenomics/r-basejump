#' Generate tx2gene from GTF File
#'
#' @rdname tx2geneFromGTF
#' @name tx2geneFromGTF
#'
#' @return [data.frame].
#'
#' @examples
#' # Mouse (Ensembl)
#' tx2geneFromGTF("http://steinbaugh.com/basejump/tests/mmusculus.gtf")
#'
#' # Fruitfly (FlyBase)
#' tx2geneFromGTF("http://steinbaugh.com/basejump/tests/dmelanogaster.gtf")
NULL



# Constructors ====
.gtfKeyValuePairs <- function(object) {
    object %>%
        .[[9L]] %>%
        unique
}



.tx2geneFromGTF <- function(object) {
    anno <- object %>%
        .gtfKeyValuePairs %>%
        .[str_detect(., "transcript_id") & str_detect(., "gene_id")] %>%
        unique

    enstxp <- str_match(anno, "transcript_id ([^;]+);") %>%
        .[, 2L]
    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]

    # Check identifier integrity
    if (!identical(length(enstxp), length(ensgene))) {
        stop("Transcript/gene mismatch")
    }

    df <- cbind(enstxp, ensgene) %>%
        as.data.frame %>%
        distinct %>%
        arrange(!!sym("enstxp")) %>%
        set_rownames(.[["enstxp"]])

    message(paste(
        "tx2gene mappings:",
        nrow(df), "transcripts,",
        length(unique(df[["ensgene"]])), "genes"))

    df
}



# Methods ====
#' @rdname tx2geneFromGTF
#' @export
setMethod("tx2geneFromGTF", "character", function(object) {
    # Check for remote file
    if (str_detect(object, "://")) {
        # Save as temp file
        file <- tempfile()
        download.file(object, file)
    } else {
        file <- object
    }
    file %>%
        readGTF %>%
        .tx2geneFromGTF
})



#' @rdname tx2geneFromGTF
#' @export
setMethod("tx2geneFromGTF", "data.frame", function(object) {
    if (dim(object)[[2L]] != 9L) {
        stop("GTF object must be data.frame with 9 columns")
    }
    .tx2geneFromGTF(object)
})
