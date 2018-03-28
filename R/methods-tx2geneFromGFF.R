#' Define Transcript to Gene Mappings from GFF/GTF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @name tx2geneFromGFF
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
#' tx2geneFromGFF(url) %>% glimpse()
#'
#' # data.frame ====
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
#' @export
setMethod(
    "tx2geneFromGFF",
    signature("data.frame"),
    function(object) {
        assertIsGFF(object)
        values <- .gffKeyValuePairs(object, unique = TRUE)
        data <- values[, c("transcriptID", "geneID")]
        # Rename `transcriptID` to `txID`
        colnames(data)[[1L]] <- "txID"
        data <- data %>%
            mutate_if(is.factor, as.character) %>%
            .[!is.na(.[["txID"]]), , drop = FALSE] %>%
            .[!is.na(.[["geneID"]]), , drop = FALSE] %>%
            unique() %>%
            .[order(.[["txID"]]), , drop = FALSE] %>%
            set_rownames(.[["txID"]])
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
