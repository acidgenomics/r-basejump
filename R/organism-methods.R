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
#' data(rse_small)
#' organism(rse_small)
NULL



.organism.string <-  # nolint
    function(object) {
        assert_is_a_string(object)
        # Get reference data from sysdata.rda.
        ref <- organism_mappings
        assert_is_tbl_df(ref)
        # Generate a logical matrix of grep matches.
        hits <- apply(
            X = ref,
            MARGIN = 1L,
            FUN = function(row) {
                any(vapply(
                    X = row,
                    FUN = function(pattern) {
                        grepl(pattern, object, ignore.case = TRUE)
                    },
                    FUN.VALUE = integer(1L)
                ))
            }
        )
        # Return organism name if there's a match, otherwise NA.
        ifelse(
            test = any(hits),
            yes = ref[hits, 1L, drop = TRUE],
            no = NA_character_
        )
    }



# Note that `character` method conflicts with `annotate` package, so we're not
# exporting this function as a method.
# We're using a while loop approach here so we can skip transgenes or spike-ins.
# Fail after 50 unknowns, for speed.
.organism.character <-  # nolint
    function(object) {
        assert_is_character(object)
        # Parse the character vector until we get a match.
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
            stop("Failed to detect organism.", call. = FALSE)
        } else {
            x
        }
    }



.organism.matrix <-  # nolint
    function(object) {
        # Assume gene identifiers are defined in the rownames.
        assertHasRownames(object)
        .organism.character(rownames(object))
    }



.organism.GRanges <-  # nolint
    function(object) {
        assert_has_names(object)
        .organism.character(names(object))
    }




# Then attempt to check rowData.
# Finally, check against the rownames.
.organism.SE <-  # nolint
    function(object) {
        # Attempt to use metadata stash, if defined.
        organism <- metadata(object)[["organism"]]
        if (is_a_string(organism)) {
            return(organism)
        }

        # Fall back to detecting from rowRanges or rownames.
        if ("geneID" %in% colnames(rowData(object))) {
            x <- as.character(rowData(object)[["geneID"]])
        } else {
            x <- rownames(object)
        }

        .organism.character(x)
    }



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
