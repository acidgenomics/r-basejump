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



# export =======================================================================
#' @rdname export
#' @export
setGeneric(
    name = "export",
    def = function(x, ...) {
        standardGeneric("export")
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
#' Return the First and Last Part of an Object
#'
#' Inspired by the [base::print()] method for `DataFrame` class objects. Applies
#' to both rows and columns, enabling quick inspection during interactive use.
#'
#' @name headtail
#' @inheritParams params
#' @export
#'
#' @return None (invisible `NULL`).
#'
#' @seealso
#' - [utils::head()], [utils::tail()], [base::cat()].
#' - `getMethod("show", "DataTable")`.
setGeneric(
    name = "headtail",
    def = function(x, ...) {
        standardGeneric("headtail")
    }
)



# interestingGroups ============================================================
#' Interesting Groups
#'
#' @name interestingGroups
#' @inheritParams params
#' @return `character`.
NULL

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
#' Interconvert Log Ratio and Fold Change Values
#'
#' @name logRatio
#' @inheritParams params
#'
#' @seealso
#' - `gtools::foldchange2logratio()`.
#' - `gtools::logratio2foldchange()`.
#'
#' @return `numeric`.
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
#' Make Syntactically Valid Names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set [base::names()], [base::rownames()], and/or [base::colnames()]
#' without modification of the values.
#'
#' `dotted.case` support is provided for matching against base R parameters.
#' However, it is recommended to avoid using it for variable assignments into an
#' `environment`, as that can introduce conflicts with base functions.
#'
#' @name makeNames
#' @inheritParams params
#'
#' @param object `character` or object for which [base::names()] assignment
#'   will be meaningful.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
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
#' Map Genes
#'
#' Take a user-defined gene vector and dynamically map the input to either the
#' object rownames or the gene names (symbols). These functions are useful for
#' writing code that needs to handle either gene identifier or gene name input
#' dynamically (e.g. for single-cell RNA-seq marker analysis).
#'
#' @section Ambiguous gene names:
#'
#' Some genomes (e.g. Homo sapiens, Mus musculus) contain duplicated gene names
#' for multiple gene identifiers. Normally we handle these ambiguous gene names
#' by sanitizing them with [base::make.names()]. If a user requests a gene name
#' that is duplicated, these functions will return a warning.
#'
#' @name mapGenes
#' @inheritParams params
#'
#' @return `character`.
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
#' Metrics
#'
#' This function takes data stored in `SummarizedExperiment::colData()` and
#' consistently returns a tibble grouped by sample by default (`sampleID`).
#'
#' [metrics()] always returns `sampleName` and `interestingGroups` columns, even
#' when these columns are not defined in `colData`. This is designed to
#' integrate with plotting functions that use ggplot2 internally.
#'
#' @name metrics
#' @inheritParams params
#'
#' @return `DataFrame`.
NULL

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



# plotCountsPerBiotype =========================================================
#' Plot Counts per Biotype
#'
#' @name plotCountsPerBiotype
#' @inheritParams params
#'
#' @return `ggplot`.
NULL

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
# TODO Move this into `plotGene()` documentation?
#' Plot Sexually Dimorphic Gender Marker Genes
#'
#' This is a convenience function that wraps [plotGene()] to quickly plot known
#' sexually dimorphic genes, and overall gene experssion for the X and Y
#' chromosomes.
#'
#' Currently only *Homo sapiens* and *Mus musculus* genomes are supported.
#'
#' @inheritParams params
#' @export
#'
#' @seealso [plotGene()].
#'
#' @return `ggplot`.
setGeneric(
    name = "plotGenderMarkers",
    def = function(object, ...) {
        standardGeneric("plotGenderMarkers")
    }
)



# plotGene =====================================================================
# TODO Rename to `plotGeneExpression()`? `plotGenes()`?
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
#' Plot Heatmap
#'
#' Construct a simple heatmap.
#'
#' @name plotHeatmap
#' @inheritParams params
#' @return `pheatmap`.
NULL

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
#' Remove Rows and Columns Containing Only NA Values
#' @inheritParams params
#' @export
#' @return Sanitized object.
setGeneric(
    name = "removeNA",
    def = function(object, ...) {
        standardGeneric("removeNA")
    }
)



# sampleData ===================================================================
#' Sample Data
#'
#' Metadata that describes the samples.
#'
#' This is a complement to the standard `SummarizedExperiment::colData()`
#' function, but improves support for accessing sample metadata for datasets
#' where multiple items in the columns map to a single sample (e.g. cells for a
#' single-cell RNA-seq experiment).
#'
#' @name sampleData
#' @inheritParams params
#'
#' @return `DataFrame`.
NULL

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
#' Sanitize NA Values
#'
#' Standardize empty strings (`""`), character NAs (`"NA"`), and `NULL` values
#' inside a character vector to `NA_character_`. Other `atomic` data types are
#' returned unmodified.
#'
#' @inheritParams params
#' @export
#'
#' @return Object containing proper `NA` values.
setGeneric(
    name = "sanitizeNA",
    def = function(object, ...) {
        standardGeneric("sanitizeNA")
    }
)



# sanitizePercent ==============================================================
#' Sanitize Percent
#'
#' Take a `character` vector of percentages (e.g. `"50%"`) and convert it to a
#' `numeric` vector (e.g. `0.5`). This function is designed primarily to
#' sanitize data imported from Microsoft Excel.
#'
#' @inheritParams params
#' @export
#'
#' @return `numeric`, if `character` and all items in the vector end with "%".
#'   Otherwise, returns the original object unmodified.
setGeneric(
    name = "sanitizePercent",
    def = function(object, ...) {
        standardGeneric("sanitizePercent")
    }
)



# selectSamples ================================================================
#' Select Samples
#'
#' Utility function that enables quick an easy sample selection in Bioconductor
#' contains that don't correspond to samples in the columns
#' (e.g. `SingleCellExperiment`).
#'
#' Internally, pattern matching against sample and file names is applied using
#' logical grep matching.
#'
#' @note Bracket based subsetting with `[` also works on `SingleCellExperiment`
#'   objects but it's not intuitive. In this case, provide cellular barcode
#'   identifiers for columns and gene identifiers for rows.
#'
#' @inheritParams params
#' @export
#'
#' @param ... Selection arguments that map to the column names of
#'   [sampleData()]. `atomic` values are supported. Avoid using `logical` or
#'   `numeric` indices (e.g. [base::which()] calls) here, for improved code
#'   readability.
#'
#' @seealso
#' - [sampleData()].
#' - [S4Vectors::split()].
#'
#' @return Modified object, with selected samples.
setGeneric(
    name = "selectSamples",
    def = function(object, ...) {
        standardGeneric("selectSamples")
    }
)



# stripTranscriptVersions ======================================================
#' Strip Transcript Versions
#'
#' @note This by default method is strict, and will only strip Ensembl
#'   transcript IDs beginning with "ENS".
#'
#' @inheritParams params
#' @export
#'
#' @return Modified object, containing transcript identifiers without version
#'   numbers.
setGeneric(
    name = "stripTranscriptVersions",
    def = function(object, ...) {
        standardGeneric("stripTranscriptVersions")
    }
)



# subsetPerSample ==============================================================
#' Subset Per Sample
#' @inheritParams params
#' @export
#' @return `SummarizedExperiment`, with selected samples.
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
#' Top Cells per Sample
#'
#' Obtain the top cellular barcodes, based on counts.
#'
#' @inheritParams params
#' @export
#'
#' @return `list`. Top barcodes as `character`, split by `sampleID`.
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
#' Unite Interesting Groups
#'
#' Create a single interesting groups column (`interestingGroups`) used for
#' coloring in plots. When multiple interesting groups are present, unite into a
#' single column, delimited by a colon.
#'
#' @inheritParams params
#' @export
#'
#' @param object Object containing column data that defines interesting groups.
#'
#' @return Modified object, containing an `interestingGroups` column.
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
