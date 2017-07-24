#' Detect the Organism from String
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
#' @param string String.
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
detectOrganism <- function(string) {
    if (str_detect(string,
                   regex("^(grch|hg)\\d{2}$", ignore_case = TRUE)) |
        str_detect(string, "^ENSG\\d{11}$")) {
        c(human = "Homo sapiens")
    } else if (str_detect(string,
                          regex("^(grcm|mm)\\d{2}$", ignore_case = TRUE)) |
               str_detect(string, "^ENSMUSG\\d{11}$")) {
        c(mouse = "Mus musculus")
    } else if (str_detect(string,
                          regex("^wbcel\\d{3}$", ignore_case = TRUE)) |
               str_detect(string, "^WBGene\\d{8}$")) {
        c(roundworm = "Caenorhabditis elegans")
    } else if (str_detect(string,
                          regex("^bdgp\\d$", ignore_case = TRUE)) |
               str_detect(string, "^FBgn\\d{7}$")) {
        c(fruitfly = "Drosophila melanogaster")
    } else if (str_detect(string, "^ENSGALG\\d{11}$")) {
        c(chicken = "Gallus gallus")
    } else if (str_detect(string, "^ENSRNOG\\d{11}$")) {
        c(rat = "Rattus norvegicus")
    } else if (str_detect(string,
                          regex("^(mb|mb)\\d", ignore_case = TRUE))) {
        warning("Unsupported genome")
        c(bacteria = "Escherichia coli")
    } else if (str_detect(string, "^ASM\\d")) {
        warning("Unsupported annotations")
        c(yeast = "Schizosaccharomyces pombe")
    } else if (str_detect(string,
                          regex("^zv\\d", ignore_case = TRUE))) {
        warning("Unsupported annotations")
        c(zebrafish = "Danio rerio")
    } else {
        stop("Failed to detect organism")
    }
}
