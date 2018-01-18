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
#' - *Danio rerio* (zebrafish)
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
#' detectOrganism("Homo_sapiens")
#' detectOrganism("H sapiens")
#' detectOrganism("H. sapiens")
#' detectOrganism("Hsapiens")
#' detectOrganism("human")
#'
#' # Unsupported organism
#' detectOrganism("XXX")
NULL



# Constructors =================================================================
.detectOrganism <- function(object) {
    # Use the first item in vector for detection
    object <- object[[1L]]
    # Homo sapiens =============================================================
    grep <- c(
        "^H(omo)?([._[:space:]]+)?sapiens$",
        "^human$",
        # Ensembl
        "^ENS(G|T)(\\d{11})$",
        "^GRCh(\\d{2})(\\.p\\d+)?$",
        # UCSC
        "^hg(\\d{2})$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(human = "Homo sapiens"))
    }

    # Mus musculus =============================================================
    grep <- c(
        "^M(us)?([._[:space:]]+)?musculus$",
        "^mouse$",
        # Ensembl
        "^ENSMUS(G|T)(\\d{11})$",
        "^GRCm(\\d{2})(\\.p\\d+)?$",
        # UCSC
        "^mm(\\d{2})$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(mouse = "Mus musculus"))
    }

    # Rattus norvegicus ====================================================
    grep <- c(
        "^R(attus)?([._[:space:]]+)?norvegicus$",
        "^rat$",
        # Ensembl
        "^ENSRNO(G|T)(\\d{11})$",
        "Rnor_([0-9\\.]+)",
        # UCSC
        "^rn(\\d+)$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(rat = "Rattus norvegicus"))
    }

    # Danio rerio ==============================================================
    grep <- c(
        "^D(anio)?([._[:space:]]+)?rerio$",
        "^zebrafish$",
        # Ensembl
        "^ENSDAR(G|T)(\\d{11})$",
        "GRCz(\\d+)",
        # UCSC
        "^danRer(\\d+)$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(zebrafish = "Danio rerio"))
    }

    # Drosophila melanogaster ==================================================
    grep <- c(
        "^D(rosophila)?([._[:space:]]+)?melanogaster$",
        "^fruitfly$",
        # Ensembl
        "^FB(gn|tr)(\\d{7})$",
        "^BDGP(\\d+)$",
        # UCSC
        "^dm(\\d+)$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(fruitfly = "Drosophila melanogaster"))
    }

    # Caenorhabditis elegans ===================================================
    grep <- c(
        "^C(aenorhabditis)?([._[:space:]]+)?elegans$",
        "^roundworm$",
        # Ensembl
        "^WBGene(\\d{8})$",
        "^WBcel(\\d{3})$",
        # UCSC
        "^ce(\\d{2})$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(roundworm = "Caenorhabditis elegans"))
    }

    # Gallus gallus ============================================================
    grep <- c(
        "G(allus)?([._[:space:]]+)?gallus$",
        "chicken$",
        # Ensembl
        "^ENSGAL(G|T)(\\d{11})$",
        "^Gallus_gallus-([0-9\\.]+)$",
        # UCSC
        "^galGal(\\d+)$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(chicken = "Gallus gallus"))
    }

    # Ovis aries ===============================================================
    grep <- c(
        "^O(vis)?([._[:space:]]+)?aries$",
        "^sheep$",
        # Ensembl
        "^ENSOAR(G|T)\\d{11}$",
        "^Oar_v([0-9\\.]+)$",
        # UCSC
        "^oviAri(\\d+)$"
    )
    if (grepl(
        x = object,
        pattern = paste(grep, collapse = "|"),
        ignore.case = TRUE
    )) {
        return(c(sheep = "Ovis aries"))
    }

    warning("Failed to detect supported organism", call. = FALSE)
    NULL
}



# Methods ======================================================================
#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("character"),
    .detectOrganism)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("NULL"),
    function(object) {
        warning("'NULL' input to 'detectOrganism()'", call. = FALSE)
        NULL
    })
