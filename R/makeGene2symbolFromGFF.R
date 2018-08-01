#' Gene-to-Symbol Mappings from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' # GTF
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gtf")
#' glimpse(x)
#'
#' # GFFv3
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gff3")
#' glimpse(x)
makeGene2symbolFromGFF <- function(file) {
    message("Making gene2symbol from GFF")
    gff <- readGFF(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

    data <- mcols(gff) %>%
        as.data.frame() %>%
        camel()
    assert_is_subset("geneID", colnames(data))
    geneIDs <- sort(unique(na.omit(data[["geneID"]])))
    data <- filter(data, !is.na(!!sym("geneID")))

    if (type == "GTF") {
        if (
            !"geneName" %in% colnames(data) &&
            "geneSymbol" %in% colnames(data)
        ) {
            # Needed for FlyBase
            data[["geneName"]] <- data[["geneSymbol"]]  # nocov
        }
    } else if (type == "GFF") {
        if (
            !"geneName" %in% colnames(data) &&
            "name" %in% colnames(data)
        ) {
            data[["geneName"]] <- data[["name"]]
        }
    }

    data <- data %>%
        select(!!!syms(c("geneID", "geneName"))) %>%
        .[complete.cases(.), , drop = FALSE] %>%
        unique() %>%
        arrange(!!sym("geneID")) %>%
        set_rownames(.[["geneID"]])

    assert_are_identical(geneIDs, rownames(data))
    data
}



# Aliases ======================================================================
#' @rdname makeGene2symbolFromGFF
#' @usage NULL
#' @export
makeGene2symbolFromGFF -> makeGene2symbolFromGTF
