#' Make Gene-to-Symbol Mappings
#'
#' @name makeGene2symbol
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @inheritParams makeGRanges
#' @inheritParams gene2symbol
#' @param ... Passthrough to [makeGRangesFromEnsembl()].
#'
#' @return `data.frame`.
#'
#' @examples
#' # makeGene2symbolFromEnsembl ====
#' x <- makeGene2symbolFromEnsembl("Homo sapiens")
#' glimpse(x)
#'
#' # makeGene2symbolFromGFF ====
#' # GTF
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gtf")
#' glimpse(x)
#' # GFF3
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gff3")
#' glimpse(x)
NULL



#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromEnsembl <- function(
    ...,
    unique = TRUE
) {
    gr <- makeGRangesFromEnsembl(..., format = "genes")
    data <- mcols(gr) %>%
        .[, c("geneID", "geneName")] %>%
        as.data.frame() %>%
        mutate_all(as.character) %>%
        arrange(!!sym("geneID")) %>%
        set_rownames(.[["geneID"]])

    # Ensure gene names (symbols) are unique, if desired.
    # This is recommended by default.
    if (isTRUE(unique)) {
        data <- .makeGeneNamesUnique(data)
    }

    assertIsGene2symbol(data)
    data
}



#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromGFF <- function(
    file,
    unique = TRUE
) {
    assert_is_a_bool(unique)

    message("Making gene2symbol from GFF")
    gff <- readGFF(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

    data <- mcols(gff) %>%
        as.data.frame() %>%
        camel()

    assert_is_subset("geneID", colnames(data))
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

    # Ensure gene names (symbols) are unique, if desired.
    # This is recommended by default.
    if (isTRUE(unique)) {
        data <- .makeGeneNamesUnique(data)
    }

    assertIsGene2symbol(data)
    data
}



#' @rdname makeGene2symbol
#' @usage NULL
#' @export
makeGene2symbolFromGFF -> makeGene2symbolFromGTF
