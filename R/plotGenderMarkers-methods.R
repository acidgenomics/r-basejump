#' Plot Sexually Dimorphic Gender Marker Genes
#'
#' This is a convenience function that wraps [plotGene()] to quickly plot known
#' sexually dimorphic genes, and overall gene experssion for the X and Y
#' chromosomes. Currently only *Homo sapiens* and *Mus musculus*
#' genomes are supported.
#'
#' @name plotGenderMarkers
#' @family Quality Control Functions
#' @author Michael Steinbaugh
#' @include plotGene-methods.R
#' @export
#'
#' @inheritParams plotGene
#' @inheritParams general
#'
#' @seealso [plotGene()].
#'
#' @return `ggplot`.
#'
#' @examples
#' # Minimal example doesn't contain these genes.
#' x <- tryCatch(
#'     expr = plotGenderMarkers(rse_small),
#'     error = function(e) e
#' )
#' print(x)
NULL



.plotGenderMarkers.SE <-  # nolint
    function() {
        validObject(object)

        # Load the relevant internal gender markers data.
        organism <- organism(object)
        markers <- basejump::gender_markers
        # Error if the organism is not supported.
        # Convert from camel case back to full Latin.
        supportedOrganisms <- names(markers) %>%
            snake() %>%
            sub("^([a-z])", "\\U\\1", ., perl = TRUE) %>%
            sub("_", " ", .)
        if (!organism %in% supportedOrganisms) {
            stop(paste0(
                organism, " is not supported.\n",
                "Supported organisms: ",
                toString(supportedOrganisms)
            ))
        }
        markers <- markers[[camel(organism)]]
        assert_is_tbl_df(markers)

        # Message the user instead of erroring, since many datasets don't
        # contain the dimorphic gender markers.
        genes <- tryCatch(
            mapGenesToRownames(
                object = object,
                genes = markers[["geneID"]]
            ),
            error = function(e) { e }
        )
        if (is(genes, "error")) {
            message(genes)
            return(invisible())
        }

        do.call(
            what = plotGene,
            args = matchArgsToDoCall(
                args = list(genes = genes)
            )
        )
    }
f <- methodFormals(f = "plotGene", signature = "SummarizedExperiment")
f <- f[setdiff(names(f), "genes")]
f[["style"]] <- "wide"
formals(.plotGenderMarkers.SE) <- f



#' @rdname plotGenderMarkers
#' @export
setMethod(
    "plotGenderMarkers",
    signature("SummarizedExperiment"),
    definition = .plotGenderMarkers.SE
)
