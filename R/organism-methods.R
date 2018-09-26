#' Organism Accessor
#'
#' Supports organism detection from Ensembl identifier or genome build.
#'
#' @section Supported organisms:
#'
#' - *Caenorhabditis elegans* (roundworm)
#' - *Danio rerio* (zebrafish)
#' - *Drosophila melanogaster* (fruitfly)
#' - *Gallus gallus* (chicken)
#' - *Homo sapiens* (human)
#' - *Mus musculus* (mouse)
#' - *Ovis aries* (sheep)
#' - *Rattus norvegicus* (rat)
#' - *Saccharomyces cerevisiae* (yeast)
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



# Prepare the organism grep match matrix.
.organismTable <- basejump::organism_mappings %>%
    mutate(
        !!sym("organismGrep") := gsub(
            pattern = "^([[:upper:]])([[:lower:]]+)[[:space:]]([[:lower:]]+)$",
            replacement = "\\1(\\2)?([._[:space:]]+)?\\3",
            x = !!sym("organism")
        )
    )



.organism.string <-  # nolint
    function(object) {
        assert_is_a_string(object)
        ref <- .organismTable
        # Generate a logical matrix of grep matches.
        mat <- apply(
            X = ref,
            MARGIN = c(1L, 2L),
            FUN = function(pattern) {
                grepl(pattern = pattern, x = object, ignore.case = TRUE)
            }
        )
        # Determine if any organism matched.
        hit <- apply(
            X = mat,
            MARGIN = 1L,
            FUN = function(x) {
                any(na.omit(x))
            }
        )
        # Return organism name if there's a match, otherwise NA.
        ifelse(
            test = any(hit),
            yes = ref[hit, 1L, drop = TRUE],
            no = NA_character_
        )
    }



# We're using a while loop approach here so we can skip transgenes or spike-ins.
# Fail after 50 unknowns, for speed.
.organism.character <-  # nolint
    function(object) {
        # Parse the vector until we get a match.
        x <- NA_character_
        i <- 1L
        while (
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
