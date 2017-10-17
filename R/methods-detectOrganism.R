#' Detect Organism
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
#' @details
#' Currently supported organisms:
#'
#' - *Homo sapiens* (human)
#' - *Mus musculus* (mouse)
#' - *Rattus norvegicus* (rat)
#' - *Drosophila melanogaster* (fruitfly)
#' - *Caenorhabditis elegans* (roundworm)
#' - *Saccharomyces cerevisiae* (yeast)
#' - *Gallus gallus* (chicken)
#' - *Ovis aries* (sheep)
#'
#' @rdname detectOrganism
#' @name detectOrganism
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Full latin organism name.
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
    # Just use the first item in vector for detection
    object <- object[[1]]
    if (grepl(
        # Homo sapiens ====
        x = object,
        pattern = paste(c(
            "^H(omo )?sapiens$",
            "^human$",
            # Ensembl
            "^ENS(G|T)(\\d{11})$",
            "^GRCh(\\d{2})(\\.p\\d+)?$",
            # UCSC
            "^hg(\\d{2})$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(human = "Homo sapiens")
    } else if (grepl(
        # Mus musculus ====
        x = object,
        pattern = paste(c(
            "^M(us )?musculus$",
            "^mouse$",
            # Ensembl
            "^ENSMUS(G|T)(\\d{11})$",
            "^GRCm(\\d{2})(\\.p\\d+)?$",
            # UCSC
            "^mm(\\d{2})$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(mouse = "Mus musculus")
    } else if (grepl(
        # Rattus norvegicus ====
        x = object,
        pattern = paste(c(
            "^R(attus )?norvegicus$",
            "^rat$",
            # Ensembl
            "^ENSRNO(G|T)(\\d{11})$",
            "Rnor_([0-9\\.]+)",
            # UCSC
            "^rn(\\d+)$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(rat = "Rattus norvegicus")
    } else if (grepl(
        # Drosophila melanogaster ====
        x = object,
        pattern = paste(c(
            "^D(rosophila )?melanogaster$",
            "^fruitfly$",
            # Ensembl
            "^FB(gn|tr)(\\d{7})$",
            "^BDGP(\\d+)$",
            # UCSC
            "^dm(\\d+)$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(fruitfly = "Drosophila melanogaster")
    } else if (grepl(
        # Caenorhabditis elegans ====
        x = object,
        pattern = paste(c(
            "^C(aenorhabditis )?elegans$",
            "^roundworm$",
            # Ensembl
            "^WBGene(\\d{8})$",
            "^WBcel(\\d{3})$",
            # UCSC
            "^ce(\\d{2})$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(roundworm = "Caenorhabditis elegans")
    } else if (grepl(
        # Gallus gallus ====
        x = object,
        pattern = paste(c(
            "G(allus )?gallus$",
            "chicken$",
            # Ensembl
            "^ENSGAL(G|T)(\\d{11})$",
            "^Gallus_gallus-([0-9\\.]+)$",
            # UCSC
            "^galGal(\\d+)$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(chicken = "Gallus gallus")
    } else if (grepl(
        # Ovis aries ====
        x = object,
        pattern = paste(c(
            "^O(vis )?aries$",
            "^sheep$",
            # Ensembl
            "^ENSOAR(G|T)\\d{11}$",
            "^Oar_v([0-9\\.]+)$",
            # UCSC
            "^oviAri(\\d+)$"
        ), collapse = "|"),
        ignore.case = TRUE
    )) {
        c(sheep = "Ovis aries")
    } else {
        stop("Failed to detect supported organism", call. = FALSE)
    }
}



# Methods ====
#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("character"),
    .detectOrganism)
