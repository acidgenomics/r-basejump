#' Detect Organism
#'
#' Supports organism detection from Ensembl identifier or genome build.
#'
#' Only the first match is returned. We're using a while loop approach here so
#' we can skip transgenes or spike-ins. The function fails after a maximum of 50
#' unknowns, for speed.
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
#' @note [organism()] `character` method conflicts with annotate package, which
#'   gets loaded into the namespace when DESeq2 is attached. Instead, we're
#'   exporting the character method here as a separate function named
#'   [detectOrganism()].
#'
#' @family Annotations
#' @author Michael Steinbaugh
#' @export
#'
#' @param object `character`.
#'
#' @return `string`. Full latin organism name. Stops on match failure.
#'
#' @seealso [organism()].
#'
#' @examples
#' # Match by gene identifier.
#' detectOrganism("ENSG00000000003")
#'
#' # Match by genome build.
#' detectOrganism("GRCh38")  # Ensembl
#' detectOrganism("hg38")    # UCSC
#'
#' # Match by alternate organism name.
#' detectOrganism("H. sapiens")
#' detectOrganism("hsapiens")
#'
#' # The function will skip transgenes/spike-ins until we find a match.
#' detectOrganism(c("EGFP", "TDTOMATO", "ENSG00000000003"))
#'
#' # But it only returns the first match, if there are multiple genomes.
#' detectOrganism(c("ENSG00000000003", "ENSMUSG00000000001"))
detectOrganism <- function(object) {
    assert_is_character(object)
    # Parse the character vector until we get a match.
    x <- NA_character_
    i <- 1L
    while (
        is.na(x) &&
        i <= min(length(object), 50L)
    ) {
        x <- .detectOrganism.string(object[[i]])
        i <- i + 1L
    }
    if (is.na(x)) {
        stop("Failed to detect organism.", call. = FALSE)
    } else {
        x
    }
}



.detectOrganism.string <-  # nolint
    function(object) {
        assert_is_a_string(object)
        # Get reference data from sysdata.rda.
        ref <- basejump::organism_mappings
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
