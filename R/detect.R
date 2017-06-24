#' Detect the organism from the genome build name
#'
#' @param genomeBuild Genome build.
#'
#' @return Organism string.
#' @export
detectOrganism <- function(genomeBuild) {
    if (str_detect(genomeBuild, "^(hg|GRCh)\\d+")) {
        "hsapiens"
    } else if (str_detect(genomeBuild, "^mm\\d+")) {
        "mmusculus"
    } else if (str_detect(genomeBuild, "^WBcel\\d+")) {
        "celegans"
    } else if (str_detect(genomeBuild, "^BDGP\\d+")) {
        "dmelanogaster"
    } else if (str_detect(genomeBuild, "^Zv\\d+")) {
        "drerio"
    } else if (str_detect(genomeBuild, "^ASM\\d+")) {
        "spombe"
    } else if (str_detect(genomeBuild, "^(MB|MG)\\d+")) {
        "ecoli"
    } else {
        stop("Failed to detect organism from genome build")
    }
}

#' @rdname detectOrganism
#' @export
detect_organism <- detectOrganism  # nolint
