#' S4 Generics
#'
#' @rdname AllGenerics
#' @name AllGenerics
#' @keywords internal
#'
#' @param object Object.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... Additional arguments.
NULL



#' @rdname annotable
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("annotable", function(object, ...) {
    standardGeneric("annotable")
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



#' @rdname createProjectDirs
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("createProjectDirs", function(object, ...) {
    standardGeneric("createProjectDirs")
})



#' @rdname dna
#' @family DNA Sequence Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("comp", function(object) {
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
setGeneric("detectOrganism", function(object) {
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



#' @rdname makeNames
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("firstCase", function(object, ...) {
    standardGeneric("firstCase")
})



#' @rdname fixNA
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("fixNA", function(object) {
    standardGeneric("fixNA")
})



#' @rdname gene2entrez
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2entrez", function(object, ...) {
    standardGeneric("gene2entrez")
})



#' @rdname gene2symbol
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2symbol", function(object, ...) {
    standardGeneric("gene2symbol")
})



#' @rdname geomean
#' @family Math Utilities
#' @inheritParams AllGenerics
setGeneric("geomean", function(object) {
    standardGeneric("geomean")
})



#' @rdname grepString
#' @family String Utilities
#' @inheritParams AllGenerics
setGeneric("grepString", function(object) {
    standardGeneric("grepString")
})



#' @rdname kables
#' @family Report Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname loadData
#' @family Data Import and Project Utilities
#' @inheritParams dots
#' @export
setGeneric("loadData", function(...) {
    standardGeneric("loadData")
})



#' @rdname loadRemoteData
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("loadRemoteData", function(object) {
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



#' @rdname packageSE
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("packageSE", function(object, ...) {
    standardGeneric("packageSE")
})



#' @rdname pct
#' @family Math Utilities
#' @inheritParams AllGenerics
setGeneric("pct", function(object) {
    standardGeneric("pct")
})



#' @rdname readFileByExtension
#' @family Data Import and Project Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
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
setGeneric("revcomp", function(object) {
    standardGeneric("revcomp")
})



#' @rdname saveData
#' @family Write Utilities
#' @inheritParams dots
#' @export
setGeneric(
    "saveData",
    signature = "...",
    function(..., dir = "data", compress = TRUE) {
        standardGeneric("saveData")
    })



#' @rdname saveDataRaw
#' @family Write Utilities
#' @inheritParams dots
#' @export
setGeneric("saveDataRaw", function(...) {
    standardGeneric("saveDataRaw")
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
setGeneric("sortUnique", function(object) {
    standardGeneric("sortUnique")
})



#' @rdname makeNames
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("titleCase", function(object, ...) {
    standardGeneric("titleCase")
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
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})



#' @rdname tx2geneFromGTF
#' @family Gene Annotation Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("tx2geneFromGTF", function(object, ...) {
    standardGeneric("tx2geneFromGTF")
})



#' @rdname wash
#' @family Cleanup Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("wash", function(object) {
    standardGeneric("wash")
})



#' @rdname writeCounts
#' @family Write Utilities
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
