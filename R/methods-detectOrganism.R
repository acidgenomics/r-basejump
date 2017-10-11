#' Detect Organism
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
#' @rdname detectOrganism
#' @name detectOrganism
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Full latin scientific organism name.
#' @export
#'
#' @examples
#' # H. sapiens
#' detectOrganism("GRCh38")
#' detectOrganism("hg38")
#' detectOrganism("ENSG00000000003")
#' detectOrganism("Homo sapiens")
#' detectOrganism("Hsapiens")
#' detectOrganism("human")
NULL



# Constructors ====
.detectOrganism <- function(object) {
    if (tolower(object) %in% c("human",
                               "hsapiens",
                               "homo sapiens") |
        str_detect(object,
                   regex("^(grch|hg)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^ENS(G|T)\\d{11}$")) {
        c(human = "Homo sapiens")
    } else if (
        tolower(object) %in% c("mouse",
                               "mmusculus",
                               "mus musculus") |
        str_detect(object,
                   regex("^(grcm|mm)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^ENSMUS(G|T)\\d{11}$")) {
        c(mouse = "Mus musculus")
    } else if (
        tolower(object) %in% c("roundworm",
                               "celegans",
                               "caenorhabditis elegans") |
        str_detect(object,
                   regex("^(ce|wbcel)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^WBGene\\d{8}$")) {
        c(roundworm = "Caenorhabditis elegans")
    } else if (
        tolower(object) %in% c("fruitfly",
                               "dmelanogaster",
                               "drosophila melanogaster") |
        str_detect(object,
                   regex("^(bdgp|dm)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^FB(gn|tr)\\d{7}$")) {
        c(fruitfly = "Drosophila melanogaster")
    } else if (
        tolower(object) %in% c("chicken",
                               "ggallus",
                               "gallus gallus") |
        str_detect(object, "^ENSGAL(G|T)\\d{11}$")) {
        c(chicken = "Gallus gallus")
    } else if (
        tolower(object) %in% c("rat",
                               "rnorvegicus",
                               "rattus norvegicus") |
        str_detect(object, "^ENSRNO(G|T)\\d{11}$")) {
        c(rat = "Rattus norvegicus")
    } else {
        stop("Failed to detect supported organism")
    }
}



# Methods ====
#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("character"),
    .detectOrganism)
