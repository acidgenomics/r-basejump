#' S4 Generics
#'
#' @rdname AllGenerics
#' @name AllGenerics
#' @keywords internal
#'
#' @param object Object.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... *Additional arguments (for the S4 generic definition).*
NULL



#' bcbio S4 Generics
#'
#' These generics are currently currently in use for both the bcbioRNASeq and
#' bcbioSingleCell packages.
#'
#' @rdname bcbioGenerics
#' @name bcbioGenerics
#' @keywords internal
#'
#' @inheritParams AllGenerics
NULL



#' @rdname aggregateFeatures
#' @export
setGeneric("aggregateFeatures", function(object, ...) {
    standardGeneric("aggregateFeatures")
})



#' @rdname aggregateReplicates
#' @export
setGeneric("aggregateReplicates", function(object, ...) {
    standardGeneric("aggregateReplicates")
})



#' @rdname annotable
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("bcbio", function(object, ...) {
    standardGeneric("bcbio")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("bcbio<-", function(object, ..., value) {
    standardGeneric("bcbio<-")
})



#' @rdname camel
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapseToString
#' @export
setGeneric("collapseToString", function(object, ...) {
    standardGeneric("collapseToString")
})



#' @rdname dna
#' @export
setGeneric("comp", function(object, ...) {
    standardGeneric("comp")
})



#' @rdname detectOrganism
#' @export
setGeneric("detectOrganism", function(object, ...) {
    standardGeneric("detectOrganism")
})



#' @rdname dots
#' @export
setGeneric(
    "dots",
    signature = "...",
    function(..., character = FALSE) {
    standardGeneric("dots")
})



#' @rdname dotted
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname logRatio
#' @export
setGeneric("fc2lr", function(object, ...) {
    standardGeneric("fc2lr")
})



#' @rdname fixNA
#' @export
setGeneric("fixNA", function(object, ...) {
    standardGeneric("fixNA")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("flatFiles", function(object, ...) {
    standardGeneric("flatFiles")
})



#' @rdname gene2symbol
#' @export
setGeneric("gene2symbol", function(object, ...) {
    standardGeneric("gene2symbol")
})



#' @rdname gene2symbolFromGFF
#' @export
setGeneric("gene2symbolFromGFF", function(object, ...) {
    standardGeneric("gene2symbolFromGFF")
})



#' @rdname geomean
setGeneric("geomean", function(object, ...) {
    standardGeneric("geomean")
})



#' @rdname grepString
setGeneric("grepString", function(object, ...) {
    standardGeneric("grepString")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("interestingGroups", function(object, ...) {
    standardGeneric("interestingGroups")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("interestingGroups<-", function(object, ..., value) {
    standardGeneric("interestingGroups<-")
})



#' @rdname kables
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname logRatio
#' @export
setGeneric("lr2fc", function(object, ...) {
    standardGeneric("lr2fc")
})



#' @rdname mdHeader
#' @export
setGeneric("mdHeader", function(object, ...) {
    standardGeneric("mdHeader")
})



#' @rdname mdList
#' @export
setGeneric("mdList", function(object, ...) {
    standardGeneric("mdList")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("metrics", function(object, ...) {
    standardGeneric("metrics")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("plotGene", function(object, ...) {
    standardGeneric("plotGene")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("plotQC", function(object, ...) {
    standardGeneric("plotQC")
})



#' @rdname prepareSummarizedExperiment
#' @export
setGeneric("prepareSummarizedExperiment", function(assays, ...) {
    standardGeneric("prepareSummarizedExperiment")
})



#' @rdname prepareTemplate
#' @export
setGeneric("prepareTemplate", function(object, ...) {
    standardGeneric("prepareTemplate")
})



#' @rdname readDataVersions
#' @export
setGeneric("readDataVersions", function(object, ...) {
    standardGeneric("readDataVersions")
})



#' @rdname readFileByExtension
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
})



#' @rdname readGFF
#' @export
setGeneric("readGFF", function(object, ...) {
    standardGeneric("readGFF")
})



#' @rdname readLogFile
#' @export
setGeneric("readLogFile", function(object, ...) {
    standardGeneric("readLogFile")
})



#' @rdname readProgramVersions
#' @export
setGeneric("readProgramVersions", function(object, ...) {
    standardGeneric("readProgramVersions")
})



#' @rdname readSampleMetadataFile
#' @export
setGeneric("readSampleMetadataFile", function(object, ...) {
    standardGeneric("readSampleMetadataFile")
})



#' @rdname readYAML
#' @export
setGeneric("readYAML", function(object, ...) {
    standardGeneric("readYAML")
})



#' @rdname removeNA
#' @export
setGeneric("removeNA", function(object, ...) {
    standardGeneric("removeNA")
})



#' @rdname dna
#' @export
setGeneric("revcomp", function(object, ...) {
    standardGeneric("revcomp")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("sampleMetadata", function(object, ...) {
    standardGeneric("sampleMetadata")
})



#' @rdname sampleYAML
#' @export
setGeneric("sampleYAML", function(yaml, keys, ...) {
    standardGeneric("sampleYAML")
})



#' @rdname sampleYAMLMetadata
#' @export
setGeneric("sampleYAMLMetadata", function(yaml, ...) {
    standardGeneric("sampleYAMLMetadata")
})



#' @rdname sampleYAMLMetrics
#' @export
setGeneric("sampleYAMLMetrics", function(yaml, ...) {
    standardGeneric("sampleYAMLMetrics")
})



#' @rdname bcbioGenerics
#' @export
setGeneric("selectSamples", function(object, ...) {
    standardGeneric("selectSamples")
})



#' @rdname snake
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @export
setGeneric("sortUnique", function(object, ...) {
    standardGeneric("sortUnique")
})



#' @rdname stripTranscriptVersions
#' @export
setGeneric("stripTranscriptVersions", function(object, ...) {
    standardGeneric("stripTranscriptVersions")
})



#' @rdname symbol2gene
#' @export
setGeneric("symbol2gene", function(object, ...) {
    standardGeneric("symbol2gene")
})



#' @rdname toStringUnique
#' @export
setGeneric("toStringUnique", function(object, ...) {
    standardGeneric("toStringUnique")
})



#' @rdname tx2gene
#' @export
setGeneric("tx2gene", function(object, ...) {
    standardGeneric("tx2gene")
})



#' @rdname tx2geneFromGFF
#' @export
setGeneric("tx2geneFromGFF", function(object, ...) {
    standardGeneric("tx2geneFromGFF")
})



#' @rdname camel
#' @export
setGeneric("upperCamel", function(object, ...) {
    standardGeneric("upperCamel")
})
