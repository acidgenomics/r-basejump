#' Detect Organism
#'
#' Supports organism detection from genome build or Ensembl identifier.
#'
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
#' @name detectOrganism
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param unique Only return unique matching organisms. Applies to character
#'   vector input.
#'
#' @return Full latin organism name. Always aborts on detection failure.
#'
#' - Vector: Named character vector containing organism name or `NA` for
#'   individual match failures (e.g. spike-ins like EGFP, TDTOMATO).
#' - Dim: Unique character vector of the organism(s) detected.  Warns if
#'   multiple organisms are detected.
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/counts.rda")
#'
#' # String
#' detectOrganism("ENSG00000000003")
#' detectOrganism("GRCh38")
#' detectOrganism("hg38")
#' detectOrganism("H. sapiens")
#'
#' # Character vector with spike-ins
#' detectOrganism(c("ENSG00000000003", "EGFP", "TDTOMATO"))
#'
#' # Matrix
#' detectOrganism(counts)
#'
#' # Tibble
#' tibble <- as(counts, "tibble")
#' detectOrganism(tibble)
NULL



# Constructors =================================================================
.detectOrganism <- function(object) {
    assert_is_a_string(object)

    # Homo sapiens =============================================================
    grep <- c(
        "^H(omo)?([._[:space:]]+)?sapiens$",
        # Ensembl
        "^ENS(G|T)(\\d{11})$",
        "^GRCh(\\d{2})(\\.p\\d+)?$",
        # UCSC
        "^hg(\\d{2})$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Homo sapiens")
    }

    # Mus musculus =============================================================
    grep <- c(
        "^M(us)?([._[:space:]]+)?musculus$",
        # Ensembl
        "^ENSMUS(G|T)(\\d{11})$",
        "^GRCm(\\d{2})(\\.p\\d+)?$",
        # UCSC
        "^mm(\\d{2})$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Mus musculus")
    }

    # Rattus norvegicus ====================================================
    grep <- c(
        "^R(attus)?([._[:space:]]+)?norvegicus$",
        # Ensembl
        "^ENSRNO(G|T)(\\d{11})$",
        "Rnor_([0-9\\.]+)",
        # UCSC
        "^rn(\\d+)$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Rattus norvegicus")
    }

    # Danio rerio ==============================================================
    grep <- c(
        "^D(anio)?([._[:space:]]+)?rerio$",
        # Ensembl
        "^ENSDAR(G|T)(\\d{11})$",
        "GRCz(\\d+)",
        # UCSC
        "^danRer(\\d+)$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Danio rerio")
    }

    # Drosophila melanogaster ==================================================
    grep <- c(
        "^D(rosophila)?([._[:space:]]+)?melanogaster$",
        # Ensembl
        "^FB(gn|tr)(\\d{7})$",
        "^BDGP(\\d+)$",
        # UCSC
        "^dm(\\d+)$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Drosophila melanogaster")
    }

    # Caenorhabditis elegans ===================================================
    grep <- c(
        "^C(aenorhabditis)?([._[:space:]]+)?elegans$",
        # Ensembl
        "^WBGene(\\d{8})$",
        "^WBcel(\\d{3})$",
        # UCSC
        "^ce(\\d{2})$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Caenorhabditis elegans")
    }

    # Gallus gallus ============================================================
    grep <- c(
        "G(allus)?([._[:space:]]+)?gallus$",
        # Ensembl
        "^ENSGAL(G|T)(\\d{11})$",
        "^Gallus_gallus-([0-9\\.]+)$",
        # UCSC
        "^galGal(\\d+)$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Gallus gallus")
    }

    # Ovis aries ===============================================================
    grep <- c(
        "^O(vis)?([._[:space:]]+)?aries$",
        # Ensembl
        "^ENSOAR(G|T)\\d{11}$",
        "^Oar_v([0-9\\.]+)$",
        # UCSC
        "^oviAri(\\d+)$"
    )
    if (grepl(paste(grep, collapse = "|"), object, ignore.case = TRUE)) {
        return("Ovis aries")
    }

    NA_character_
}



.detectOrganism.character <- function(object, unique = FALSE) {  # nolint
    assert_is_a_bool(unique)
    x <- vapply(
        X = object,
        FUN = .detectOrganism,
        FUN.VALUE = character(1L)
    )
    if (all(is.na(x))) {
        stop("Failed to detect organism")
    }
    if (is_a_string(x)) {
        names(x) <- NULL
    }
    if (length(na.omit(unique(x))) > 1L) {
        warning("Multiple organisms detected")
    }
    if (isTRUE(unique)) {
        x <- x %>%
            as.character() %>%
            na.omit() %>%
            unique()
    }
    x
}



.detectOrganism.dim <- function(object) {  # nolint
    # Assume gene identifiers are defined in the rownames
    assertHasRownames(object)
    .returnUniqueOrganism(rownames(object))
}



.detectOrganism.tibble <- function(object) {  # nolint
    assert_has_colnames(object)
    object <- camel(object)
    idCols <- c("rowname", "geneID", "ensemblGeneID", "ensgene")
    assert_are_intersecting_sets(idCols, colnames(object))
    idCol <- match(
        x = idCols,
        table = colnames(object)
    ) %>%
        na.omit() %>%
        .[[1L]]
    .returnUniqueOrganism(object[, idCol, drop = TRUE])
}



.returnUniqueOrganism <- function(object) {
    assert_is_character(object)
    x <- detectOrganism(object)
    x <- sort(unique(na.omit(x)))
    x
}



# Methods ======================================================================
#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("character"),
    .detectOrganism.character
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("data.frame"),
    .detectOrganism.dim
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("DataFrame"),
    .detectOrganism.dim
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("dgCMatrix"),
    .detectOrganism.dim
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("matrix"),
    .detectOrganism.dim
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("tbl_df"),
    .detectOrganism.tibble
)
