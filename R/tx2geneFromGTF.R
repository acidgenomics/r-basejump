#' Generate tx2gene from GTF File
#'
#' @rdname tx2geneFromGTF
#' @name tx2geneFromGTF
#'
#' @param save Save tx2gene to disk as RData and CSV files.
#'
#' @return [data.frame].
NULL



# Constructors ====
.tx2geneFromGTF <- function(object, save = TRUE) {
    gtf <- read_tsv(object, col_names = FALSE)
    enstxp <- str_extract(gtf$X9, "FBtr\\d{7}")
    ensgene <- str_extract(gtf$X9, "FBgn\\d{7}")
    tx2gene <- cbind(enstxp, ensgene) %>%
        as.data.frame %>%
        filter(!is.na(enstxp)) %>%
        distinct %>%
        arrange(enstxp) %>%
        set_rownames(.$enstxp)
    if (isTRUE(save)) {
        saveData(tx2gene)
        dir.create("annotations", showWarnings = FALSE)
        write_csv(tx2gene, file.path("annotations", "tx2gene.csv"))
    }
    tx2gene
}



# Methods ====
setMethod("tx2geneFromGTF", "character", .tx2geneFromGTF)
