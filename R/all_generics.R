#' S4 Generics
#'
#' @rdname all_generics
#' @name all_generics
#' @keywords internal
#'
#' @param object Object.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... Additional arguments.
NULL



#' @rdname annotables
#' @inheritParams all_generics
#' @export
setGeneric("annotable", function(object) {
    standardGeneric("annotable")
})



#' @rdname assignAndSaveData
#' @inheritParams all_generics
#' @export
setGeneric("assignAndSaveData", function(name, object, ...) {
    standardGeneric("assignAndSaveData")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname collapse
#' @inheritParams all_generics
#' @export
setGeneric("collapse", function(object, ...) {
    standardGeneric("collapse")
})



#' @rdname dna
#' @inheritParams all_generics
#' @export
setGeneric("comp", function(object) {
    standardGeneric("comp")
})



#' @rdname detectOrganism
#' @inheritParams all_generics
#' @export
setGeneric("detectOrganism", function(object) {
    standardGeneric("detectOrganism")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname logRatio
#' @inheritParams all_generics
#' @export
setGeneric("fc2lr", function(object, ...) {
    standardGeneric("fc2lr")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("firstCase", function(object, ...) {
    standardGeneric("firstCase")
})



#' @rdname fixNA
#' @inheritParams all_generics
#' @export
setGeneric("fixNA", function(object) {
    standardGeneric("fixNA")
})



#' @rdname annotables
#' @inheritParams all_generics
#' @export
setGeneric("gene2entrez", function(object) {
    standardGeneric("gene2entrez")
})



#' @rdname annotables
#' @inheritParams all_generics
#' @export
setGeneric("gene2symbol", function(object) {
    standardGeneric("gene2symbol")
})



#' @rdname geomean
#' @inheritParams all_generics
#' @family Math Utilities
setGeneric("geomean", function(object) {
    standardGeneric("geomean")
})



#' @rdname grepString
#' @inheritParams all_generics
setGeneric("grepString", function(object) {
    standardGeneric("grepString")
})



#' @rdname kables
#' @inheritParams all_generics
#' @export
setGeneric("kables", function(object, ...) {
    standardGeneric("kables")
})



#' @rdname load
#' @inheritParams all_generics
#' @export
setGeneric("loadRemote", function(object) {
    standardGeneric("loadRemote")
})



#' @rdname logRatio
#' @inheritParams all_generics
#' @export
setGeneric("lr2fc", function(object, ...) {
    standardGeneric("lr2fc")
})



#' @rdname mdHeader
#' @family Markdown Utilities
#' @inheritParams all_generics
#' @export
setGeneric("mdHeader", function(object, ...) {
    standardGeneric("mdHeader")
})



#' @rdname mdList
#' @family Markdown Utilities
#' @inheritParams all_generics
#' @export
setGeneric("mdList", function(object, ...) {
    standardGeneric("mdList")
})



#' @rdname geomean
#' @inheritParams all_generics
#' @family Math Utilities
setGeneric("pct", function(object) {
    standardGeneric("pct")
})



#' @rdname readFileByExtension
#' @inheritParams all_generics
#' @export
setGeneric("readFileByExtension", function(object, ...) {
    standardGeneric("readFileByExtension")
})



#' @rdname readYAML
#' @inheritParams all_generics
#' @export
setGeneric("readYAML", function(object, ...) {
    standardGeneric("readYAML")
})



#' @rdname dna
#' @inheritParams all_generics
#' @export
setGeneric("removeNA", function(object, ...) {
    standardGeneric("removeNA")
})



#' @rdname dna
#' @inheritParams all_generics
#' @export
setGeneric("revcomp", function(object) {
    standardGeneric("revcomp")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname sortUnique
#' @inheritParams all_generics
#' @export
setGeneric("sortUnique", function(object) {
    standardGeneric("sortUnique")
})



#' @rdname collapse
#' @inheritParams all_generics
#' @export
setGeneric("summarizeRows", function(object, ...) {
    standardGeneric("summarizeRows")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("titleCase", function(object, ...) {
    standardGeneric("titleCase")
})



#' @rdname transmit
#' @inheritParams all_generics
#' @export
setGeneric("transmit", function(object, ...) {
    standardGeneric("transmit")
})



#' @rdname annotables
#' @inheritParams all_generics
#' @export
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})



#' @rdname wash
#' @inheritParams all_generics
#' @export
setGeneric("wash", function(object) {
    standardGeneric("wash")
})
