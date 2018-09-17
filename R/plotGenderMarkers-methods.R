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



.plotGenderMarkers.RSE <-  # nolint
    function(object, style = "wide") {
        validObject(object)
        # This works but should be more automatic...see below
        call <- standardizeCall()
        definition <- sys.function(sys.parent(n = 2L))

        # Load the relevant internal gender markers data.
        organism <- organism(object)
        markers <- basejump::gender_markers
        assert_is_subset(camel(organism), names(markers))
        markers <- markers[[camel(organism)]]
        assert_is_tbl_df(markers)

        # Get the X and Y chromosome marker genes
        xGenes <- markers %>%
            filter(!!sym("chromosome") == "X") %>%
            pull("geneID")
        yGenes <- markers %>%
            filter(!!sym("chromosome") == "Y") %>%
            pull("geneID")

        do.call(
            what = plotGene,
            args = matchArgsToDoCall(
                args = list(genes = c(xGenes, yGenes)),
                # FIXME This doesn't seem to be picking up definition
                # (e.g. style) correctly
                # n = 2L,  # 2-4?
                definition = definition,
                call = call,
                verbose = TRUE
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
