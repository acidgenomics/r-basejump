#' Detect Organism
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
#' @rdname detectOrganism
#' @name detectOrganism
#'
#' @return Full latin scientific organism name.
#' @export
#'
#' @examples
#' # H. sapiens
#' detectOrganism("GRCh38")
#' detectOrganism("hg38")
#' detectOrganism("ENSG00000000003")
#' detectOrganism("human")
#' detectOrganism("hsapiens")
#' detectOrganism("Homo sapiens")
#'
#' # M. musculus
#' detectOrganism("GRCm38")
#' detectOrganism("mm10")
#' detectOrganism("ENSMUSG00000002459")
#'
#' # C. elegans
#' detectOrganism("WBcel235")
#' detectOrganism("ce11")
#' detectOrganism("WBGene00000001")
#'
#' # D. melanogaster
#' detectOrganism("BDGP6")
#' detectOrganism("dm6")
#' detectOrganism("FBgn0000003")
NULL



# Methods ====
#' @rdname detectOrganism
#' @export
setMethod("detectOrganism", "character", function(object) {
    if (object %in% c("human",
                      "hsapiens",
                      "Homo sapiens") |
        str_detect(object,
                   regex("^(grch|hg)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^ENS(G|T)\\d{11}$")) {
        c(human = "Homo sapiens")
    } else if (
        object %in% c("mouse",
                      "mmusculus",
                      "Mus musculus") |
        str_detect(object,
                   regex("^(grcm|mm)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^ENSMUS(G|T)\\d{11}$")) {
        c(mouse = "Mus musculus")
    } else if (
        object %in% c("roundworm",
                      "celegans",
                      "Caenorhabditis elegans") |
        str_detect(object,
                   regex("^(ce|wbcel)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^WBGene\\d{8}$")) {
        c(roundworm = "Caenorhabditis elegans")
    } else if (
        object %in% c("fruitfly",
                      "dmelanogaster",
                      "Drosophila melanogaster") |
        str_detect(object,
                   regex("^(bdgp|dm)\\d+$", ignore_case = TRUE)) |
        str_detect(object, "^FB(gn|tr)\\d{7}$")) {
        c(fruitfly = "Drosophila melanogaster")
    } else if (
        object %in% c("chicken",
                      "ggallus",
                      "Gallus gallus") |
        str_detect(object, "^ENSGAL(G|T)\\d{11}$")) {
        c(chicken = "Gallus gallus")
    } else if (
        object %in% c("rat",
                      "rnorvegicus",
                      "Rattus norvegicus") |
        str_detect(object, "^ENSRNO(G|T)\\d{11}$")) {
        c(rat = "Rattus norvegicus")
    } else {
        stop("Failed to detect supported organism")
    }
})
