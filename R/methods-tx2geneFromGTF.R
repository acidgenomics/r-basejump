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
.tx2geneFromGTF <- function(object, save = FALSE) {
    first <- read_lines(object, n_max = 1L)
    if (str_detect(first, "^#!genome-build")) {
        message("Ensembl GTF")
        # The first 5 lines are comments
        gtf <- read_tsv(object, col_names = FALSE, skip = 5L)
        anno <- gtf[["X9"]] %>%
            unique %>%
            # Check for transcript identifier
            str_subset("ENS[A-Z]+T\\d{11}") %>%
            # Check for gene identifier
            str_subset("ENS.+G\\d{11}") %>%
            unique
        enstxp <- str_extract(anno, "ENS[A-Z]+T\\d{11}")
        ensgene <- str_extract(anno, "ENS.+G\\d{11}")
    } else if (str_detect(first, "FlyBase")) {
        message("FlyBase GTF")
        gtf <- read_tsv(object, col_names = FALSE)
        anno <- gtf[["X9"]] %>%
            unique %>%
            # Check for transcript identifier
            str_subset("FBtr\\d{7}") %>%
            # Check for gene identifier
            str_subset("FBgn\\d{7}") %>%
            unique
        enstxp <- str_extract(anno, "FBtr\\d{7}")
        ensgene <- str_extract(anno, "FBgn\\d{7}")
    } else {
        stop("Unsupported GTF format")
    }

    # Unload GTF from memory
    rm(gtf)

    # Check identifier integrity
    if (!length(enstxp)) {
        stop("Unexpected transcript identifier match failure")
    }
    if (length(enstxp) != length(ensgene)) {
        stop("Transcript/gene identifier mismatch")
    }

    tx2gene <- cbind(enstxp, ensgene) %>%
        as.data.frame %>%
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
#' @rdname tx2geneFromGTF
#' @export
setMethod("tx2geneFromGTF", "character", .tx2geneFromGTF)
