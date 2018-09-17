# FIXME Need to improve the formals to match `plotGene()`.



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



# FIXME Plot all X and Y genes.
# FIXME Move this to basejump and check for RSE.
.plotGenderMarkers.RSE <-  # nolint
    function(object, ...) {
        validObject(object)
        # FIXME Check if we need this...
        # rse <- as(object, "RangedSummarizedExperiment")
        style <- "wide"

        # Load the relevant internal gender markers data.
        organism <- organism(object)
        markers <- basejump::gender_markers
        assert_is_subset(camel(organism), names(markers))
        markers <- markers[[camel(organism)]]

        # Require that object contains X and Y chromosome annotations.
        assert_is_subset(c("X", "Y"), seqnames(object))

        # Known dimorphic X chromosome markers.
        xKnownPlot <- do.call(
            what = plotGene,
            args = list(
                object = object,
                genes = markers %>%
                    filter(!!sym("chromosome") == "X") %>%
                    pull("geneID"),
                style = style
                # FIXME
                # ...
            )
        ) +
            ggtitle("X known")

        # Known dimorphic Y chromosome markers.
        yKnownPlot <- do.call(
            what = plotGene,
            args = list(
                object = object,
                genes = markers %>%
                    filter(!!sym("chromosome") == "Y") %>%
                    pull("geneID"),
                style = style
                # FIXME
                # ...
            )
        ) +
            ggtitle("Y known")

        # All genes on X and Y chromosome.

        plotlist <- list(x = xPlot, y = yPlot)
        plot_grid(plotlist = plotlist)
    }



#' @rdname plotGenderMarkers
#' @export
setMethod(
    "plotGenderMarkers",
    signature("RangedSummarizedExperiment"),
    definition = .plotGenderMarkers.RSE
)
