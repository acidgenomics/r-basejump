#' Detect Organism
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
#' @rdname detectOrganism
#'
#' @return Full latin scientific organism name.
#' @export
#'
#' @examples
#' # H. sapiens
#' detectOrganism("hg38")
#' detectOrganism("ENSG00000000003")
#'
#' # M. musculus
#' detectOrganism("mm10")
#' detectOrganism("ENSMUSG00000002459")
#'
#' # C. elegans
#' detectOrganism("WBcel235")
#' detectOrganism("WBGene00000001")
#'
#' # D. melanogaster
#' detectOrganism("BDGP6")
#' detectOrganism("FBgn0000003")
setMethod("detectOrganism", "character", function(object) {
    if (str_detect(object,
                   regex("^(grch|hg)\\d{2}$", ignore_case = TRUE)) |
        str_detect(object, "^ENS(G|T)\\d{11}$")) {
        c(human = "Homo sapiens")
    } else if (str_detect(object,
                          regex("^(grcm|mm)\\d{2}$", ignore_case = TRUE)) |
               str_detect(object, "^ENSMUS(G|T)\\d{11}$")) {
        c(mouse = "Mus musculus")
    } else if (str_detect(object,
                          regex("^wbcel\\d{3}$", ignore_case = TRUE)) |
               str_detect(object, "^WBGene\\d{8}$")) {
        c(roundworm = "Caenorhabditis elegans")
    } else if (str_detect(object,
                          regex("^bdgp\\d$", ignore_case = TRUE)) |
               str_detect(object, "^FB(gn|tr)\\d{7}$")) {
        c(fruitfly = "Drosophila melanogaster")
    } else if (str_detect(object, "^ENSGAL(G|T)\\d{11}$")) {
        c(chicken = "Gallus gallus")
    } else if (str_detect(object, "^ENSRNO(G|T)\\d{11}$")) {
        c(rat = "Rattus norvegicus")
    } else if (str_detect(object,
                          regex("^(mb|mb)\\d", ignore_case = TRUE))) {
        warning("Unsupported genome")
        c(bacteria = "Escherichia coli")
    } else if (str_detect(object, "^ASM\\d")) {
        warning("Unsupported annotations")
        c(yeast = "Schizosaccharomyces pombe")
    } else if (str_detect(object,
                          regex("^zv\\d", ignore_case = TRUE))) {
        warning("Unsupported annotations")
        c(zebrafish = "Danio rerio")
    } else {
        stop("Failed to detect organism")
    }
})
