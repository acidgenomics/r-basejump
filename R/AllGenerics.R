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



#' @rdname AllGenerics
#' @export
setGeneric("aggregateReplicates", function(object, ...) {
    standardGeneric("aggregateReplicates")
})




#' @rdname annotable
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
})



#' @rdname AllGenerics
#' @export
setGeneric("bcbio", function(object, ...) {
    standardGeneric("bcbio")
})



#' @rdname AllGenerics
#' @export
setGeneric("bcbio<-", function(object, ..., value) {
    standardGeneric("bcbio<-")
})



#' @rdname makeNames
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapse
#' @family Data Manipulation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("collapse", function(object, ...) {
    standardGeneric("collapse")
})



#' @rdname dna
#' @family DNA Sequence Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("comp", function(object, ...) {
    standardGeneric("comp")
})



#' @rdname detectHPC
#' @family Infrastructure Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("detectHPC", function(object, ...) {
    standardGeneric("detectHPC")
})



#' @rdname detectOrganism
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("detectOrganism", function(object, ...) {
    standardGeneric("detectOrganism")
})



#' @rdname dots
#' @family Function Utilities
#' @param ... Objects as dots.
#' @export
setGeneric(
    "dots",
    signature = "...",
    function(..., character = FALSE) {
    standardGeneric("dots")
})



#' @rdname makeNames
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname logRatio
#' @family Math Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("fc2lr", function(object, ...) {
    standardGeneric("fc2lr")
})



#' @rdname fixNA
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("fixNA", function(object, ...) {
    standardGeneric("fixNA")
})



#' @rdname gene2symbol
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2symbol", function(object, ...) {
    standardGeneric("gene2symbol")
})



#' @rdname gene2symbolFromGTF
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2symbolFromGTF", function(object, ...) {
    standardGeneric("gene2symbolFromGTF")
})



#' @rdname geomean
#' @family Math Utilities
#' @inheritParams AllGenerics
setGeneric("geomean", function(object, ...) {
    standardGeneric("geomean")
})



#' @rdname grepString
#' @family String Utilities
#' @inheritParams AllGenerics
setGeneric("grepString", function(object, ...) {
    standardGeneric("grepString")
})



#' @rdname AllGenerics
#' @export
setGeneric("interestingGroups", function(object) {
    standardGeneric("interestingGroups")
})



#' @rdname kables
#' @family Report Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname loadRemoteData
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("loadRemoteData", function(object, ...) {
    standardGeneric("loadRemoteData")
})



#' @rdname logRatio
#' @family Math Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("lr2fc", function(object, ...) {
    standardGeneric("lr2fc")
})



#' @rdname mdHeader
#' @family Report Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("mdHeader", function(object, ...) {
    standardGeneric("mdHeader")
})



#' @rdname mdList
#' @family Report Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("mdList", function(object, ...) {
    standardGeneric("mdList")
})



#' @rdname AllGenerics
#' @export
setGeneric("metadataTable", function(object, ...) {
    standardGeneric("metadataTable")
})



#' @rdname AllGenerics
#' @export
setGeneric("metrics", function(object) {
    standardGeneric("metrics")
})



#' @rdname pct
#' @family Math Utilities
#' @inheritParams AllGenerics
setGeneric("pct", function(object, ...) {
    standardGeneric("pct")
})



#' @rdname AllGenerics
#' @export
setGeneric("plotGene", function(object, ...) {
    standardGeneric("plotGene")
})



#' @rdname prepareSummarizedExperiment
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("prepareSummarizedExperiment", function(object, ...) {
    standardGeneric("prepareSummarizedExperiment")
})



#' @rdname prepareTemplate
#' @family Infrastructure Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("prepareTemplate", function(object, ...) {
    standardGeneric("prepareTemplate")
})



#' @rdname readFileByExtension
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
})



#' @rdname readGTF
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readGTF", function(object, ...) {
    standardGeneric("readGTF")
})



#' @rdname readYAML
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readYAML", function(object, ...) {
    standardGeneric("readYAML")
})



#' @rdname removeNA
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("removeNA", function(object, ...) {
    standardGeneric("removeNA")
})



#' @rdname dna
#' @family DNA Sequence Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("revcomp", function(object, ...) {
    standardGeneric("revcomp")
})



#' @rdname AllGenerics
#' @export
setGeneric("sampleDirs", function(object) {
    standardGeneric("sampleDirs")
})



#' @rdname AllGenerics
#' @export
setGeneric("sampleMetadata", function(object, ...) {
    standardGeneric("sampleMetadata")
})



#' @rdname AllGenerics
#' @export
setGeneric("selectSamples", function(object, ...) {
    standardGeneric("selectSamples")
})



#' @rdname makeNames
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("sortUnique", function(object, ...) {
    standardGeneric("sortUnique")
})



#' @rdname symbol2gene
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("symbol2gene", function(object, ...) {
    standardGeneric("symbol2gene")
})



#' @rdname transmit
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("transmit", function(object, ...) {
    standardGeneric("transmit")
})



#' @rdname tx2gene
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("tx2gene", function(object, ...) {
    standardGeneric("tx2gene")
})



#' @rdname tx2geneFromGTF
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("tx2geneFromGTF", function(object, ...) {
    standardGeneric("tx2geneFromGTF")
})



#' @rdname writeCounts
#' @family Write Utilities
#' @author Michael Steinbaugh, Rory Kirchner
#' @inheritParams dots
#' @export
setGeneric(
    "writeCounts",
    signature = "...",
    function(...,
             dir = file.path("results", "counts"),
             gzip = TRUE) {
        standardGeneric("writeCounts")
    })
