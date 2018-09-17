#' Organism Accessor
#'
#' Supports organism detection from Ensembl identifier or genome build.
#'
#' @section Supported organisms:
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
#' @name organism
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @importFrom BiocGenerics organism
#' @export
#'
#' @inheritParams general
#'
#' @return `string`. Full latin organism name. Stops on match failure.
#'
#' @examples
#' # Match by gene identifier.
#' organism("ENSG00000000003")
#'
#' # Match by genome build.
#' organism("GRCh38")  # Ensembl
#' organism("hg38")    # UCSC
#'
#' # Match by alternate organism name.
#' organism("H. sapiens")
#' organism("hsapiens")
#'
#' # The function will skip transgenes/spike-ins until we find a match.
#' organism(c("EGFP", "TDTOMATO", "ENSG00000000003"))
#'
#' # But it only returns the first match, if there are multiple genomes.
#' organism(c("ENSG00000000003", "ENSMUSG00000000001"))
#'
#' # SummarizedExperiment support.
#' organism(rse_small)
NULL



.organism.string <-  # nolint
    function(object) {
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



# We're using a while loop approach here so we can skip transgenes or spike-ins.
# Fail after 50 unknowns, for speed.
.organism.character <-  # nolint
    function(object) {
        # Parse the vector until we get a match.
        x <- NA_character_
        i <- 1L
        while(
            is.na(x) &&
            i <= min(length(object), 50L)
        ) {
            x <- .organism.string(object[[i]])
            i <- i + 1L
        }
        if (is.na(x)) {
            stop("Failed to detect organism")
        } else {
            x
        }
    }



.organism.matrix <-  # nolint
    function(object) {
        # Assume gene identifiers are defined in the rownames.
        assertHasRownames(object)
        organism(rownames(object))
    }



.organism.GRanges <-  # nolint
    function(object) {
        assert_has_names(object)
        organism(names(object))
    }



# Attempt to use metadata stash first.
# Then attempt to check rowData.
# Finally, check against the rownames.
.organism.SE <-  # nolint
    function(object) {
        organism <- metadata(object)[["organism"]]
        if (is_a_string(organism)) {
            organism
        } else if ("geneID" %in% colnames(rowData(object))) {
            organism(rowData(object)[["geneID"]])
        } else {
            organism(rownames(object))
        }
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("character"),
    definition = .organism.character
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("factor"),
    definition = getMethod(
        f = "organism",
        signature = signature("character")
    )
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("matrix"),
    definition = .organism.matrix
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("data.frame"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("DataFrame"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("sparseMatrix"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("GRanges"),
    definition = .organism.GRanges
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("SummarizedExperiment"),
    definition = .organism.SE
)
