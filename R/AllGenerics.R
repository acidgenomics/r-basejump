# Ensembl2Entrez ===============================================================
#' @rdname Ensembl2Entrez
#' @export
setGeneric(
    name = "Ensembl2Entrez",
    def = function(object, ...) {
        standardGeneric("Ensembl2Entrez")
    }
)



# Gene2Symbol ==================================================================
#' @rdname Gene2Symbol
#' @export
setGeneric(
    name = "Gene2Symbol",
    def = function(object, ...) {
        standardGeneric("Gene2Symbol")
    }
)



# Tx2Gene ======================================================================
#' @rdname Tx2Gene
#' @export
setGeneric(
    name = "Tx2Gene",
    def = function(object, ...) {
        standardGeneric("Tx2Gene")
    }
)



# aggregate ====================================================================
#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateRows",
    def = function(object, ...) {
        standardGeneric("aggregateRows")
    }
)

#' @rdname aggregate
#' @export
setGeneric(
    name = "aggregateCols",
    def = function(object, ...) {
        standardGeneric("aggregateCols")
    }
)

#' @rdname aggregateCellsToSamples
#' @export
setGeneric(
    name = "aggregateCellsToSamples",
    def = function(object, ...) {
        standardGeneric("aggregateCellsToSamples")
    }
)



# alphaSummary =================================================================
#' Alpha level cutoff summary statistics
#'
#' Quickly generate a summary table of various alpha level cutoffs.
#'
#' @inheritParams params
#' @export
#'
#' @return `integer matrix`.
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(alphaSummary)
setGeneric(
    name = "alphaSummary",
    def = function(object, ...) {
        standardGeneric("alphaSummary")
    }
)



# autopadZeros =================================================================
#' @rdname autopadZeros
#' @export
setGeneric(
    name = "autopadZeros",
    def = function(object, ...) {
        standardGeneric("autopadZeros")
    }
)



# barcodeRanksPerSample ========================================================
#' Barcode ranks per sample
#'
#' @inheritParams params
#' @export
#'
#' @return `list`.
#'
#' @seealso
#' - [DropletUtils](https://bioconductor.org/packages/DropletUtils/).
#' - [SingleCellExperiment](https://bioconductor.org/packages/SingleCellExperiment/).
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(barcodeRanksPerSample)
setGeneric(
    name = "barcodeRanksPerSample",
    def = function(object, ...) {
        standardGeneric("barcodeRanksPerSample")
    }
)



# cell2sample ==================================================================
#' @rdname cell2sample
#' @export
setGeneric(
    name = "cell2sample",
    def = function(object, ...) {
        standardGeneric("cell2sample")
    }
)



# collapseToString =============================================================
#' @rdname collapseToString
#' @export
setGeneric(
    name = "collapseToString",
    def = function(object, ...) {
        standardGeneric("collapseToString")
    }
)



# contrastName =================================================================
#' Contrast name
#'
#' @inheritParams params
#' @export
#'
#' @return `character(1)`.
#' Contrast name.
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(contrastName)
setGeneric(
    name = "contrastName",
    def = function(object, ...) {
        standardGeneric("contrastName")
    }
)



# convertGenesToSymbols ========================================================
#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    name = "convertGenesToSymbols",
    def = function(object, ...) {
        standardGeneric("convertGenesToSymbols")
    }
)

#' @rdname convertGenesToSymbols
#' @export
setGeneric(
    name = "convertSymbolsToGenes",
    def = function(object, ...) {
        standardGeneric("convertSymbolsToGenes")
    }
)



# convertSampleIDsToNames ======================================================
#' @rdname convertSampleIDsToNames
#' @export
setGeneric(
    name = "convertSampleIDsToNames",
    def = function(object, ...) {
        standardGeneric("convertSampleIDsToNames")
    }
)



# convertTranscriptsToGenes ====================================================
#' @rdname convertTranscriptsToGenes
#' @export
setGeneric(
    name = "convertTranscriptsToGenes",
    def = function(object, ...) {
        standardGeneric("convertTranscriptsToGenes")
    }
)



# deg ==========================================================================
#' Differentially expressed genes
#'
#' @inheritParams params
#' @export
#'
#' @return `character`.
#' Gene identifiers.
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(deg)
setGeneric(
    name = "deg",
    def = function(object, ...) {
        standardGeneric("deg")
    }
)



# encode =======================================================================
#' @rdname encode
#' @export
setGeneric(
    name = "encode",
    def = function(x, ...) {
        standardGeneric("encode")
    }
)



# export =======================================================================
#' @rdname export
#' @export
setGeneric(
    name = "export",
    def = function(x, ...) {
        standardGeneric("export")
    }
)



# filterCells ==================================================================
#' Filter cells
#'
#' @inheritParams params
#' @export
#'
#' @return `SingleCellExperiment`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(filterCells)
setGeneric(
    name = "filterCells",
    def = function(object, ...) {
        standardGeneric("filterCells")
    }
)



# flatFiles ====================================================================
#' @rdname coerceS4ToList
#' @export
setGeneric(
    name = "flatFiles",
    def = function(object, ...) {
        standardGeneric("flatFiles")
    }
)



# geneNames ====================================================================
#' @rdname geneNames
#' @export
setGeneric(
    name = "geneNames",
    def = function(object, ...) {
        standardGeneric("geneNames")
    }
)



# geometricMean ================================================================
#' @rdname geometricMean
#' @export
setGeneric(
    name = "geometricMean",
    def = function(object, ...) {
        standardGeneric("geometricMean")
    }
)



# headtail =====================================================================
#' @rdname headtail
#' @export
setGeneric(
    name = "headtail",
    def = function(x, ...) {
        standardGeneric("headtail")
    }
)



# humanize =====================================================================
#' @rdname humanize
#' @export
setGeneric(
    name = "humanize",
    def = function(object, ...) {
        standardGeneric("humanize")
    }
)



# interestingGroups ============================================================
#' @rdname interestingGroups
#' @export
setGeneric(
    name = "interestingGroups",
    def = function(object, ...) {
        standardGeneric("interestingGroups")
    }
)

#' @rdname interestingGroups
#' @export
setGeneric(
    name = "interestingGroups<-",
    def = function(object, ..., value) {
        standardGeneric("interestingGroups<-")
    }
)



# logRatio =====================================================================
#' @rdname logRatio
#' @name logRatio
NULL

#' @rdname logRatio
#' @export
setGeneric(
    name = "foldChangeToLogRatio",
    def = function(object, ...) {
        standardGeneric("foldChangeToLogRatio")
    }
)

#' @rdname logRatio
#' @export
setGeneric(
    name = "logRatioToFoldChange",
    def = function(object, ...) {
        standardGeneric("logRatioToFoldChange")
    }
)



# makeNames ====================================================================
#' @rdname makeNames
#' @name makeNames
NULL

#' @rdname makeNames
#' @export
setGeneric(
    name = "camel",
    def = function(object, ...) {
        standardGeneric("camel")
    }
)

#' @rdname makeNames
#' @export
setGeneric(
    name = "dotted",
    def = function(object, ...) {
        standardGeneric("dotted")
    }
)

#' @rdname makeNames
#' @export
setGeneric(
    name = "snake",
    def = function(object, ...) {
        standardGeneric("snake")
    }
)

#' @rdname makeNames
#' @export
setGeneric(
    name = "upperCamel",
    def = function(object, ...) {
        standardGeneric("upperCamel")
    }
)



# mapGenes =====================================================================
#' @rdname mapGenes
#' @name mapGenes
NULL

#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToRownames",
    def = function(object, ...) {
        standardGeneric("mapGenesToRownames")
    }
)

#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToIDs",
    def = function(object, ...) {
        standardGeneric("mapGenesToIDs")
    }
)

#' @rdname mapGenes
#' @export
setGeneric(
    name = "mapGenesToSymbols",
    def = function(object, ...) {
        standardGeneric("mapGenesToSymbols")
    }
)



# markdown =====================================================================
#' @rdname markdown
#' @export
setGeneric(
    name = "markdown",
    def = function(object, ...) {
        standardGeneric("markdown")
    }
)



# meltCounts ===================================================================
#' @rdname meltCounts
#' @export
setGeneric(
    name = "meltCounts",
    def = function(object, ...) {
        standardGeneric("meltCounts")
    }
)



# metrics ======================================================================
#' @rdname metrics
#' @export
setGeneric(
    name = "metrics",
    def = function(object, ...) {
        standardGeneric("metrics")
    }
)

#' @rdname metrics
#' @export
setGeneric(
    name = "metricsPerSample",
    def = function(object, ...) {
        standardGeneric("metricsPerSample")
    }
)



# plot5Prime3PrimeBias =========================================================
#' Plot 5' to 3' bias
#'
#' RNA-seq data can have specific biases at either the 5’ or 3’ end of sequenced
#' fragments.
#'
#' It is common to see a small amount of bias, especially if polyA enrichment
#' was performed, or if there is any sample degradation. If a large amount of
#' bias is observed here, be sure to analyze the samples with a Bioanalyzer and
#' check the RIN scores.
#'
#' 5' (3') bias is generally calculated as the median of the following ratio:
#'
#' ```
#' [mean expression of 5' (3')] / [mean expression of whole transcript]
#' ```
#'
#' For example:
#'
#' - Mean expression for 5' (3') is calculated as mean coverage of first (last)
#'   100 bases.
#' - Mean expression of transcript is the mean coverage of all bases in that
#'   transcript.
#' - Median is calculated for the representative set of 1000 transcripts.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plot5Prime3PrimeBias)
setGeneric(
    name = "plot5Prime3PrimeBias",
    def = function(object, ...) {
        standardGeneric("plot5Prime3PrimeBias")
    }
)



# plotBarcodeRanks =============================================================
#' Plot barcode ranks
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot` grid.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotBarcodeRanks)
setGeneric(
    name = "plotBarcodeRanks",
    def = function(object, ...) {
        standardGeneric("plotBarcodeRanks")
    }
)



# plotCellCounts ===============================================================
#' Plot cell counts
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotCellCounts)
setGeneric(
    name = "plotCellCounts",
    def = function(object, ...) {
        standardGeneric("plotCellCounts")
    }
)



# plotCountsPerBiotype =========================================================
#' @rdname plotCountsPerBiotype
#' @export
setGeneric(
    name = "plotCountsPerBiotype",
    def = function(object, ...) {
        standardGeneric("plotCountsPerBiotype")
    }
)

#' @rdname plotCountsPerBiotype
#' @export
setGeneric(
    name = "plotCountsPerBroadClass",
    def = function(object, ...) {
        standardGeneric("plotCountsPerBroadClass")
    }
)



# plotCountsPerGene ============================================================
#' @rdname plotCountsPerGene
#' @export
setGeneric(
    name = "plotCountsPerGene",
    def = function(object, ...) {
        standardGeneric("plotCountsPerGene")
    }
)



# plotDEGHeatmap ===============================================================
#' Differentially expressed gene heatmap
#'
#' This function is an extension of `plotHeatmap` that is optimized
#' for automatic handling differentially expressed genes, rather than requiring
#' manual input of a gene vector. All of the optional parameters for
#' `plotHeatmap` are also supported by this function.
#'
#' To adjust the annotation columns, modify the
#' [colData][SummarizedExperiment::colData] of the `counts` argument, which must
#' contain a `SummarizedExperiment`.
#'
#' @inherit plotHeatmap params return
#' @inheritParams params
#' @export
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(plotDEGHeatmap)
setGeneric(
    name = "plotDEGHeatmap",
    def = function(object, ...) {
        standardGeneric("plotDEGHeatmap")
    }
)



# plotDEGPCA ===================================================================
#' Plot differentially expressed gene principal component analysis
#'
#' This function is an extension of [plotPCA()] that is optimized for automatic
#' handling of differentially expressed genes, rather than requiring manual
#' input of a gene vector or subset object. All of the optional parameters for
#' [plotPCA()] are also supported by this function.
#'
#' To adjust the annotation columns, modify the
#' [colData][SummarizedExperiment::colData] of the `counts` argument, which must
#' contain/extend a `SummarizedExperiment`.
#'
#' @inherit plotPCA params return
#' @inheritParams params
#' @export
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(plotDEGPCA)
setGeneric(
    name = "plotDEGPCA",
    def = function(object, ...) {
        standardGeneric("plotDEGPCA")
    }
)



# plotExonicMappingRate ========================================================
#' Plot exonic mapping rate
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/)
#'
#' @examples
#' isS4(plotExonicMappingRate)
setGeneric(
    name = "plotExonicMappingRate",
    def = function(object, ...) {
        standardGeneric("plotExonicMappingRate")
    }
)



# plotGenderMarkers ============================================================
#' @rdname plotGenderMarkers
#' @export
setGeneric(
    name = "plotGenderMarkers",
    def = function(object, ...) {
        standardGeneric("plotGenderMarkers")
    }
)



# plotGene =====================================================================
#' @rdname plotGene
#' @export
setGeneric(
    name = "plotGene",
    def = function(object, ...) {
        standardGeneric("plotGene")
    }
)



# plotGeneSaturation ===========================================================
#' Plot gene detection saturation
#'
#' We should observe a linear trend in the number of genes detected with the
#' number of mapped reads, which indicates that the sample input was not
#' overloaded.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotGeneSaturation)
setGeneric(
    name = "plotGeneSaturation",
    def = function(object, ...) {
        standardGeneric("plotGeneSaturation")
    }
)



# plotGenesPerCell =============================================================
#' Plot genes per cell
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotGenesPerCell)
setGeneric(
    name = "plotGenesPerCell",
    def = function(object, ...) {
        standardGeneric("plotGenesPerCell")
    }
)



# plotGenesDetected ============================================================
#' @rdname plotGenesDetected
#' @export
setGeneric(
    name = "plotGenesDetected",
    def = function(object, ...) {
        standardGeneric("plotGenesDetected")
    }
)



# plotHeatmap ==================================================================
#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotHeatmap",
    def = function(object, ...) {
        standardGeneric("plotHeatmap")
    }
)

#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotCorrelationHeatmap",
    def = function(object, ...) {
        standardGeneric("plotCorrelationHeatmap")
    }
)

#' @rdname plotHeatmap
#' @export
setGeneric(
    name = "plotQuantileHeatmap",
    def = function(object, ...) {
        standardGeneric("plotQuantileHeatmap")
    }
)



# plotIntronicMappingRate ======================================================
#' Plot intronic mapping rate
#'
#' The majority of reads should map to exons and not introns.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotIntronicMappingRate)
setGeneric(
    name = "plotIntronicMappingRate",
    def = function(object, ...) {
        standardGeneric("plotIntronicMappingRate")
    }
)



# plotMappedReads ==============================================================
#' Plot mapped reads
#'
#' The number of mapped reads should correspond to the number of total reads.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotMappedReads)
setGeneric(
    name = "plotMappedReads",
    def = function(object, ...) {
        standardGeneric("plotMappedReads")
    }
)



# plotMappingRate ==============================================================
#' Plot mapping rate
#'
#' The genomic mapping rate represents the percentage of reads mapping to the
#' reference genome. Low mapping rates are indicative of sample contamination,
#' poor sequencing quality or other artifacts.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotMappingRate)
setGeneric(
    name = "plotMappingRate",
    def = function(object, ...) {
        standardGeneric("plotMappingRate")
    }
)



# plotMeanSD ===================================================================
#' Plot row standard deviations vs. row means
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(plotMeanSD)
setGeneric(
    name = "plotMeanSD",
    def = function(object, ...) {
        standardGeneric("plotMeanSD")
    }
)



# plotMitoRatio ================================================================
#' Plot mitochondrial transcript abundance
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotMitoRatio)
setGeneric(
    name = "plotMitoRatio",
    def = function(object, ...) {
        standardGeneric("plotMitoRatio")
    }
)



# plotMitoVsCoding =============================================================
#' Plot mitochondrial vs. coding counts
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotMitoVsCoding)
setGeneric(
    name = "plotMitoVsCoding",
    def = function(object, ...) {
        standardGeneric("plotMitoVsCoding")
    }
)



# plotNovelty ==================================================================
#' Plot novelty score
#'
#' "Novelty" refers to log10 genes detected per count.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotNovelty)
setGeneric(
    name = "plotNovelty",
    def = function(object, ...) {
        standardGeneric("plotNovelty")
    }
)



# plotQC =======================================================================
#' @rdname plotQC
#' @export
setGeneric(
    name = "plotQC",
    def = function(object, ...) {
        standardGeneric("plotQC")
    }
)



# plotRRNAMappingRate ==========================================================
#' Plot ribosomal RNA (rRNA) mapping rate
#'
#' Clean, high-quality samples should have an rRNA mapping rate below 10%.
#' Higher rates are likely indicative of the polyA enrichment or ribo depletion
#' protocol not having removed all ribosomal RNA (rRNA) transcripts. This will
#' reduce the number of biologically meaningful reads in the experiment and is
#' best avoided.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotRRNAMappingRate)
setGeneric(
    name = "plotRRNAMappingRate",
    def = function(object, ...) {
        standardGeneric("plotRRNAMappingRate")
    }
)



# plotReadsPerCell =============================================================
#' Plot read counts per cell
#'
#' Plot the distribution of read counts for all unfiltered cellular barcodes.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' [bcbioSingleCell](https://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#'
#' @examples
#' isS4(plotReadsPerCell)
setGeneric(
    name = "plotReadsPerCell",
    def = function(object, ...) {
        standardGeneric("plotReadsPerCell")
    }
)



# plotTotalCounts ==============================================================
#' @rdname plotTotalCounts
#' @export
setGeneric(
    name = "plotTotalCounts",
    def = function(object, ...) {
        standardGeneric("plotTotalCounts")
    }
)



# plotTotalReads ===============================================================
#' Plot total reads
#'
#' High quality RNA-seq samples ideally should have at least 10 million reads
#' per sample.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @examples
#' isS4(plotUMIsVsGenes)
setGeneric(
    name = "plotTotalReads",
    def = function(object, ...) {
        standardGeneric("plotTotalReads")
    }
)



# plotUMIsPerCell ==============================================================
#' Plot UMIs per cell
#'
#' Plot the universal molecular identifiers (UMIs) per cell.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotUMIsVsGenes)
setGeneric(
    name = "plotUMIsPerCell",
    def = function(object, ...) {
        standardGeneric("plotUMIsPerCell")
    }
)



# plotUMIsVsGenes ==============================================================
#' Plot UMI and gene correlation
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso
#' - [bcbioSingleCell](http://bioinformatics.sph.harvard.edu/bcbioSingleCell/).
#' - [Chromium](https://github.com/steinbaugh/Chromium/).
#'
#' @examples
#' isS4(plotUMIsVsGenes)
setGeneric(
    name = "plotUMIsVsGenes",
    def = function(object, ...) {
        standardGeneric("plotUMIsVsGenes")
    }
)



# plotVolcano ==================================================================
#' Volcano plot
#'
#' @note Don't apply post hoc alpha level or LFC threshold cutoffs.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @examples
#' isS4(plotVolcano)
setGeneric(
    name = "plotVolcano",
    def = function(object, ...) {
        standardGeneric("plotVolcano")
    }
)



# plotZerosVsDepth =============================================================
#' @rdname plotZerosVsDepth
#' @export
setGeneric(
    name = "plotZerosVsDepth",
    def = function(object, ...) {
        standardGeneric("plotZerosVsDepth")
    }
)



# relativeLogExpression ========================================================
#' Relative log expression
#'
#' @inheritParams params
#' @export
#'
#' @references Anders and Huber (2010).
#'
#' @seealso
#' - `edgeR::calcNormFactors`.
#' - [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @return `matrix`.
setGeneric(
    name = "relativeLogExpression",
    def = function(object, ...) {
        standardGeneric("relativeLogExpression")
    }
)



# removeNA =====================================================================
#' @rdname removeNA
#' @export
setGeneric(
    name = "removeNA",
    def = function(object, ...) {
        standardGeneric("removeNA")
    }
)



# sampleData ===================================================================
#' @rdname sampleData
#' @export
setGeneric(
    name = "sampleData",
    def = function(object, ...) {
        standardGeneric("sampleData")
    }
)

#' @rdname sampleData
#' @export
setGeneric(
    name = "sampleData<-",
    def = function(object, ..., value) {
        standardGeneric("sampleData<-")
    }
)



# sanitizeNA ===================================================================
#' @rdname sanitizeNA
#' @export
setGeneric(
    name = "sanitizeNA",
    def = function(object, ...) {
        standardGeneric("sanitizeNA")
    }
)



# sanitizePercent ==============================================================
#' @rdname sanitizePercent
#' @export
setGeneric(
    name = "sanitizePercent",
    def = function(object, ...) {
        standardGeneric("sanitizePercent")
    }
)



# selectSamples ================================================================
#' @rdname selectSamples
#' @export
setGeneric(
    name = "selectSamples",
    def = function(object, ...) {
        standardGeneric("selectSamples")
    }
)



# stripTranscriptVersions ======================================================
#' @rdname stripTranscriptVersions
#' @export
setGeneric(
    name = "stripTranscriptVersions",
    def = function(object, ...) {
        standardGeneric("stripTranscriptVersions")
    }
)



# subsetPerSample ==============================================================
#' @rdname subsetPerSample
#' @export
setGeneric(
    name = "subsetPerSample",
    def = function(object, ...) {
        standardGeneric("subsetPerSample")
    }
)



# tmm ==========================================================================
#' Trimmed mean of M-values
#'
#' TMM normalization is recommended for RNA-seq data generally when the majority
#' of genes are not differentially expressed.
#'
#' @note Only recommended for gene-level counts.
#'
#' @inheritParams params
#' @export
#'
#' @references Robinson and Oshlack (2010).
#'
#' @seealso
#' - `edgeR::calcNormFactors`.
#' - `edgeR::cpm`.
#' - [bcbioRNASeq](https://bioinformatics.sph.harvard.edu/bcbioRNASeq/).
#'
#' @return `matrix`.
setGeneric(
    name = "tmm",
    def = function(object, ...) {
        standardGeneric("tmm")
    }
)



# topCellsPerSample ============================================================
#' @rdname topCellsPerSample
#' @export
setGeneric(
    name = "topCellsPerSample",
    def = function(object, ...) {
        standardGeneric("topCellsPerSample")
    }
)



# topTables ====================================================================
#' Top tables of differentially expressed genes
#'
#' @inheritParams params
#' @export
#'
#' @seealso [DESeqAnalysis](https://steinbaugh.com/DESeqAnalysis/).
#'
#' @return `kable`.
#' Markdown tables.
setGeneric(
    name = "topTables",
    def = function(object, ...) {
        standardGeneric("topTables")
    }
)



# uniteInterestingGroups =======================================================
#' @rdname uniteInterestingGroups
#' @export
setGeneric(
    name = "uniteInterestingGroups",
    def = function(object, ...) {
        standardGeneric("uniteInterestingGroups")
    }
)



# zerosVsDepth =================================================================
#' @rdname zerosVsDepth
#' @export
setGeneric(
    name = "zerosVsDepth",
    def = function(object, ...) {
        standardGeneric("zerosVsDepth")
    }
)
