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



#' @rdname annotables
#' @inheritParams AllGenerics
#' @export
setGeneric("annotable", function(object) {
    standardGeneric("annotable")
})



#' @rdname makeNames
#' @inheritParams AllGenerics
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapse
#' @inheritParams AllGenerics
#' @export
setGeneric("collapse", function(object, ...) {
    standardGeneric("collapse")
})



#' @rdname dna
#' @inheritParams AllGenerics
#' @export
setGeneric("comp", function(object) {
    standardGeneric("comp")
})



#' @rdname detectOrganism
#' @inheritParams AllGenerics
#' @export
setGeneric("detectOrganism", function(object) {
    standardGeneric("detectOrganism")
})



#' @rdname dots
#' @param ... Objects as dots.
#' @export
setGeneric(
    "dots",
    signature = "...",
    function(..., character = FALSE) {
    standardGeneric("dots")
})



#' @rdname makeNames
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
#' @inheritParams AllGenerics
#' @export
setGeneric("firstCase", function(object, ...) {
    standardGeneric("firstCase")
})



#' @rdname fixNA
#' @inheritParams AllGenerics
#' @export
setGeneric("fixNA", function(object) {
    standardGeneric("fixNA")
})



#' @rdname annotables
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2entrez", function(object) {
    standardGeneric("gene2entrez")
})



#' @rdname annotables
#' @inheritParams AllGenerics
#' @export
setGeneric("gene2symbol", function(object) {
    standardGeneric("gene2symbol")
})



#' @rdname geomean
#' @inheritParams AllGenerics
#' @family Math Utilities
setGeneric("geomean", function(object) {
    standardGeneric("geomean")
})



#' @rdname grepString
#' @inheritParams AllGenerics
setGeneric("grepString", function(object) {
    standardGeneric("grepString")
})



#' @rdname kables
#' @family Markdown Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname loadData
#' @family Load Utilities
#' @inheritParams dots
#' @export
setGeneric("loadData", function(...) {
    standardGeneric("loadData")
})



#' @rdname loadRemoteData
#' @family Load Utilities
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
#' @family Markdown Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("mdHeader", function(object, ...) {
    standardGeneric("mdHeader")
})



#' @rdname mdList
#' @family Markdown Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("mdList", function(object, ...) {
    standardGeneric("mdList")
})



#' @rdname pct
#' @inheritParams AllGenerics
#' @family Math Utilities
setGeneric("pct", function(object) {
    standardGeneric("pct")
})



#' @rdname readFileByExtension
#' @family Read Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
})



#' @rdname readYAML
#' @family Read Utilities
#' @inheritParams AllGenerics
#' @export
setGeneric("readYAML", function(object, ...) {
    standardGeneric("readYAML")
})



#' @rdname removeNA
#' @inheritParams AllGenerics
#' @export
setGeneric("removeNA", function(object, ...) {
    standardGeneric("removeNA")
})



#' @rdname dna
#' @inheritParams AllGenerics
#' @export
setGeneric("revcomp", function(object) {
    standardGeneric("revcomp")
})



#' @rdname saveData
#' @family Save Utilities
#' @inheritParams dots
#' @export
setGeneric(
    "saveData",
    signature = "...",
    function(..., dir = "data", compress = TRUE) {
        standardGeneric("saveData")
    })



#' @rdname saveDataRaw
#' @family Save Utilities
#' @inheritParams dots
#' @export
setGeneric("saveDataRaw", function(...) {
    standardGeneric("saveDataRaw")
})



#' @rdname makeNames
#' @inheritParams AllGenerics
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @inheritParams AllGenerics
#' @export
setGeneric("sortUnique", function(object) {
    standardGeneric("sortUnique")
})



#' @rdname makeNames
#' @inheritParams AllGenerics
#' @export
setGeneric("titleCase", function(object, ...) {
    standardGeneric("titleCase")
})



#' @rdname transmit
#' @inheritParams AllGenerics
#' @export
setGeneric("transmit", function(object, ...) {
    standardGeneric("transmit")
})



#' @rdname annotables
#' @inheritParams AllGenerics
#' @export
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})



#' @rdname wash
#' @inheritParams AllGenerics
#' @export
setGeneric("wash", function(object) {
    standardGeneric("wash")
})



#' @rdname writeCounts
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
