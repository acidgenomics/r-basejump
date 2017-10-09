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



#' @rdname annotable
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
})



#' @rdname makeNames
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapse
#' @export
setGeneric("collapse", function(object, ...) {
    standardGeneric("collapse")
})



#' @rdname dna
#' @export
setGeneric("comp", function(object, ...) {
    standardGeneric("comp")
})



#' @rdname detectHPC
#' @export
setGeneric("detectHPC", function(object, ...) {
    standardGeneric("detectHPC")
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



#' @rdname makeNames
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



#' @rdname gene2symbolFromGFF
#' @export
setGeneric("gene2symbolFromGTF", function(object, ...) {
    standardGeneric("gene2symbolFromGTF")
})



#' @rdname geomean
setGeneric("geomean", function(object, ...) {
    standardGeneric("geomean")
})



#' @rdname grepString
setGeneric("grepString", function(object, ...) {
    standardGeneric("grepString")
})



#' @rdname kables
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname loadRemoteData
#' @export
setGeneric("loadRemoteData", function(object, ...) {
    standardGeneric("loadRemoteData")
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



#' @rdname pct
setGeneric("pct", function(object, ...) {
    standardGeneric("pct")
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



#' @rdname readGFF
#' @export
setGeneric("readGTF", function(object, ...) {
    standardGeneric("readGTF")
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



#' @rdname makeNames
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @export
setGeneric("sortUnique", function(object, ...) {
    standardGeneric("sortUnique")
})



#' @rdname symbol2gene
#' @export
setGeneric("symbol2gene", function(object, ...) {
    standardGeneric("symbol2gene")
})



#' @rdname transmit
#' @export
setGeneric("transmit", function(object, ...) {
    standardGeneric("transmit")
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



#' @rdname tx2geneFromGFF
#' @export
setGeneric("tx2geneFromGTF", function(object, ...) {
    standardGeneric("tx2geneFromGTF")
})



#' @rdname writeCounts
#' @export
setGeneric(
    "writeCounts",
    signature = "...",
    function(...,
             dir = file.path("results", "counts"),
             gzip = TRUE) {
        standardGeneric("writeCounts")
    })
