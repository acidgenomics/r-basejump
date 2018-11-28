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
#' Alpha Level Cutoff Summary Statistics
#'
#' Quickly generate a summary table of various alpha level cutoffs.
#'
#' @inheritParams params
#' @export
#'
#' @return `integer matrix`.
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
#' Barcode Ranks per Sample
#' @inheritParams params
#' @export
#' @return `list`.
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
#' Contrast Name
#'
#' @inheritParams params
#' @export
#'
#' @return `string`. Contrast name.
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
#' Filter Cells
#' @inheritParams params
#' @export
#' @return `SingleCellExperiment`.
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
#' Markdown
#' @inheritParams params
#' @export
#' @return Markdown output.
setGeneric(
    name = "markdown",
    def = function(object, ...) {
        standardGeneric("markdown")
    }
)



# meltCounts ===================================================================
#' Melt Count Matrix to Long Format
#' @inheritParams params
#' @seealso `reshape2::melt()`.
#' @export
#' @return `tibble`, grouped by sample.
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
#' Plot 5'->3' Bias
#'
#' RNA-seq data can have specific biases at either the 5’ or 3’ end of sequenced
#' fragments.
#'
#' It is common to see a small amount of bias, especially if polyA
#' enrichment was performed, or if there is any sample degradation. If a large
#' amount of bias is observed here, be sure to analyze the samples with a
#' Bioanalyzer and check the RIN scores.
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
#' @export
#' @inheritParams params
#'
#' @return `ggplot`.
setGeneric(
    name = "plot5Prime3PrimeBias",
    def = function(object, ...) {
        standardGeneric("plot5Prime3PrimeBias")
    }
)



# plotBarcodeRanks =============================================================
#' Plot Barcode Ranks
#' @inheritParams params
#' @export
#' @return `ggplot` grid.
setGeneric(
    name = "plotBarcodeRanks",
    def = function(object, ...) {
        standardGeneric("plotBarcodeRanks")
    }
)



# plotCellCounts ===============================================================
#' Plot Cell Counts
#' @inheritParams params
#' @export
#' @return `ggplot`.
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
#' Plot Counts Per Gene
#'
#' Generally, we expect similar count spreads for all genes between samples
#' unless the library sizes or total RNA expression are different.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
setGeneric(
    name = "plotCountsPerGene",
    def = function(object, ...) {
        standardGeneric("plotCountsPerGene")
    }
)



# plotDEGHeatmap ===============================================================
#' Differentially Expressed Gene Heatmap
#'
#' @details
#' This function is a simplified version of `plotHeatmap()` that is optimized
#' for automatic handling differentially expressed genes, rather than requiring
#' manual input of a gene vector. All of the optional parameters for
#' `plotHeatmap` are also available to this function.
#'
#' To adjust the annotation columns, modify the `colData` of the `counts`
#' argument, which must contain/extend a `SummarizedExperiment`.
#'
#' @note Post hoc alpha level and LFC threshold cutoffs are not recommended.
#'
#' @inherit plotHeatmap
#' @inheritParams params
#' @export
setGeneric(
    name = "plotDEGHeatmap",
    def = function(object, ...) {
        standardGeneric("plotDEGHeatmap")
    }
)



# plotDEGPCA ===================================================================
#' Plot DEG PCA
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotDEGPCA",
    def = function(object, ...) {
        standardGeneric("plotDEGPCA")
    }
)



# plotExonicMappingRate ========================================================
#' Plot Exonic Mapping Rate
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotExonicMappingRate",
    def = function(object, ...) {
        standardGeneric("plotExonicMappingRate")
    }
)



# plotGenderMarkers ============================================================
#' Plot Sexually Dimorphic Gender Marker Genes
#'
#' This is a convenience function that wraps `plotGene()` to quickly plot known
#' sexually dimorphic genes, and overall gene experssion for the X and Y
#' chromosomes.
#'
#' Currently only *Homo sapiens* and *Mus musculus* genomes are supported.
#'
#' @inheritParams params
#' @export
#'
#' @seealso `plotGene()`.
#'
#' @return `ggplot`.
setGeneric(
    name = "plotGenderMarkers",
    def = function(object, ...) {
        standardGeneric("plotGenderMarkers")
    }
)



# plotGene =====================================================================
#' Plot Gene Expression
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotGene",
    def = function(object, ...) {
        standardGeneric("plotGene")
    }
)



# plotGeneSaturation ===========================================================
#' Plot Gene Detection Saturation
#'
#' We should observe a linear trend in the number of genes detected with the
#' number of mapped reads, which indicates that the sample input was not
#' overloaded.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotGeneSaturation",
    def = function(object, ...) {
        standardGeneric("plotGeneSaturation")
    }
)



# plotGenesPerCell =============================================================
#' Plot Genes per Cell
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotGenesPerCell",
    def = function(object, ...) {
        standardGeneric("plotGenesPerCell")
    }
)



# plotGenesDetected ============================================================
#' Plot Genes Detected
#' @inheritParams params
#' @export
#' @return `ggplot`.
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
#' Plot Intronic Mapping Rate
#'
#' The majority of reads should map to exons and not introns.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotIntronicMappingRate",
    def = function(object, ...) {
        standardGeneric("plotIntronicMappingRate")
    }
)



# plotMappedReads ==============================================================
#' Plot Mapped Reads
#'
#' The number of mapped reads should correspond to the number of total reads.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotMappedReads",
    def = function(object, ...) {
        standardGeneric("plotMappedReads")
    }
)



# plotMappingRate ==============================================================
#' Plot Mapping Rate
#'
#' The genomic mapping rate represents the percentage of reads mapping to the
#' reference genome. Low mapping rates are indicative of sample contamination,
#' poor sequencing quality or other artifacts.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotMappingRate",
    def = function(object, ...) {
        standardGeneric("plotMappingRate")
    }
)



# plotMeanSD ===================================================================
#' Plot Row Standard Deviations vs. Row Means
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotMeanSD",
    def = function(object, ...) {
        standardGeneric("plotMeanSD")
    }
)



# plotMitoRatio ================================================================
#' Plot Mitochondrial Transcript Abundance
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotMitoRatio",
    def = function(object, ...) {
        standardGeneric("plotMitoRatio")
    }
)



# plotMitoVsCoding =============================================================
#' Plot Mitochondrial vs. Coding Counts
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotMitoVsCoding",
    def = function(object, ...) {
        standardGeneric("plotMitoVsCoding")
    }
)



# plotNovelty ==================================================================
#' Plot Novelty Score
#'
#' "Novelty" refers to log10 genes detected per count.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
setGeneric(
    name = "plotNovelty",
    def = function(object, ...) {
        standardGeneric("plotNovelty")
    }
)



# plotQC =======================================================================
#' Plot Quality Control
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotQC",
    def = function(object, ...) {
        standardGeneric("plotQC")
    }
)



# plotRRNAMappingRate ==========================================================
#' Plot Ribosomal RNA (rRNA) Mapping Rate
#'
#' Clean, high-quality samples should have an rRNA mapping rate below 10%.
#' Higher rates are likely indicative of the polyA enrichment or ribo depletion
#' protocol not having removed all ribosomal RNA (rRNA) transcripts. This will
#' reduce the number of biologically meaningful reads in the experiment and is
#' best avoided.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotRRNAMappingRate",
    def = function(object, ...) {
        standardGeneric("plotRRNAMappingRate")
    }
)



# plotReadsPerCell =============================================================
#' Plot Read Counts per Cell
#'
#' Plot the distribution of read counts for all unfiltered cellular barcodes.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
setGeneric(
    name = "plotReadsPerCell",
    def = function(object, ...) {
        standardGeneric("plotReadsPerCell")
    }
)



# plotTotalCounts ==============================================================
#' Plot Total Counts
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotTotalCounts",
    def = function(object, ...) {
        standardGeneric("plotTotalCounts")
    }
)



# plotTotalReads ===============================================================
# TODO Need to clarify difference between "total counts" and "total reads".
#' Plot Total Reads
#'
#' High quality RNA-seq samples ideally should have at least 10 million reads
#' per sample.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotTotalReads",
    def = function(object, ...) {
        standardGeneric("plotTotalReads")
    }
)



# plotUMIsPerCell ==============================================================
#' Plot UMIs per Cell
#'
#' Plot the universal molecular identifiers (UMIs) per cell.
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
setGeneric(
    name = "plotUMIsPerCell",
    def = function(object, ...) {
        standardGeneric("plotUMIsPerCell")
    }
)



# plotUMIsVsGenes ==============================================================
#' Plot UMI and Gene Correlation
#'
#' @inheritParams params
#' @export
#'
#' @return `ggplot`.
setGeneric(
    name = "plotUMIsVsGenes",
    def = function(object, ...) {
        standardGeneric("plotUMIsVsGenes")
    }
)



# plotVolcano ==================================================================
#' Plot Volcano
#'
#' @note Don't apply post hoc alpha level or LFC threshold cutoffs.
#'
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotVolcano",
    def = function(object, ...) {
        standardGeneric("plotVolcano")
    }
)



# plotZerosVsDepth =============================================================
#' Plot Percentage of Zeros vs. Library Depth
#' @inheritParams params
#' @export
#' @return `ggplot`.
setGeneric(
    name = "plotZerosVsDepth",
    def = function(object, ...) {
        standardGeneric("plotZerosVsDepth")
    }
)



# relativeLogExpression ========================================================
# TODO Improve citation link here.
#' Relative Log Expression
#'
#' @inheritParams params
#' @export
#'
#' @seealso `edgeR::calcNormFactors()`.
#'
#' @references Anders and Huber (2010).
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
#' Trimmed Mean of M-Values
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
#' - `edgeR::calcNormFactors()`.
#' - `edgeR::cpm()`.
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
#' Top Tables of Differential Expression Results
#' @inheritParams params
#' @export
#' @return `kable`. Markdown tables.
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
