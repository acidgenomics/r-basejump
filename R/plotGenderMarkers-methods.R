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
#' # bcbioRNASeq ====
#' object <- bcb_small
#' plotGenderMarkers(object, normalized = "vst")
#'
#' # DESeqTransform ====
#' object <- as(deseq_small, "DESeqTransform")
#' plotGenderMarkers(object)
NULL



# FIXME Improve error message if not human or mouse.
.plotGenderMarkers.RSE <-  # nolint
    function(object, style = "wide") {
        validObject(object)
        # Load the relevant internal gender markers data.
        organism <- organism(object)
        markers <- basejump::gender_markers
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
        do.call(
            what = plotGene,
            args = matchArgsToDoCall(
                args = list(genes = markers[["geneID"]])
            )
        )
    }
f1 <- formals(.plotGenderMarkers.RSE)
f2 <- methodFormals(f = "plotGene", signature = "SummarizedExperiment")
f2 <- f2[setdiff(names(f2), c(names(f1), "genes"))]
f <- c(f1, f2)
formals(.plotGenderMarkers.RSE) <- f



#' @rdname plotGenderMarkers
#' @export
setMethod(
    "plotGenderMarkers",
    signature("RangedSummarizedExperiment"),
    definition = .plotGenderMarkers.RSE
)
