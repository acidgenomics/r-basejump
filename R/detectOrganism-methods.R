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
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param unique `boolean`. Only return unique matching organisms. Applies to
#'   `character` input.
#'
#' @return Full latin organism name. Stops on detection failure.
#'
#' - `atomic`: Named `character` vector containing organism name or `NA` for
#'   individual match failures (e.g. spike-ins like EGFP, TDTOMATO).
#' - `dim`: Unique `character` vector of the organism(s) detected. Warns if
#'   multiple organisms are detected.
#'
#' @examples
#' # By gene identifier
#' detectOrganism("ENSG00000000003")
#' detectOrganism(c("ENSG00000000003", "EGFP", "TDTOMATO"))
#'
#' # By genome build
#' detectOrganism("GRCh38")  # Ensembl
#' detectOrganism("hg38")    # UCSC
#'
#' # By alternate organism name
#' detectOrganism("H. sapiens")
#' detectOrganism("hsapiens")
NULL



.detectOrganism <- function(object) {
    assert_is_a_string(object)

    # Homo sapiens
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

    # Mus musculus
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

    # Rattus norvegicus
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

    # Danio rerio
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

    # Drosophila melanogaster
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

    # Caenorhabditis elegans
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

    # Gallus gallus
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

    # Ovis aries
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



.returnUniqueOrganism <- function(object) {
    assert_is_character(object)
    x <- detectOrganism(object)
    x <- sort(unique(na.omit(x)))
    x
}



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("character"),
    function(object, unique = FALSE) {
        assert_is_a_bool(unique)
        x <- vapply(
            X = as.character(object),
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
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("factor"),
    getMethod("detectOrganism", "character")
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("matrix"),
    function(object) {
        # Assume gene identifiers are defined in the rownames
        assertHasRownames(object)
        .returnUniqueOrganism(rownames(object))
    }
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("data.frame"),
    getMethod("detectOrganism", "matrix")
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("DataFrame"),
    getMethod("detectOrganism", "matrix")
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("dgCMatrix"),
    getMethod("detectOrganism", "matrix")
)



#' @rdname detectOrganism
#' @export
setMethod(
    "detectOrganism",
    signature("tbl_df"),
    function(object) {
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
)
