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



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("firstCase", function(object, ...) {
    standardGeneric("firstCase")
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



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname names
#' @inheritParams all_generics
#' @export
setGeneric("titleCase", function(object, ...) {
    standardGeneric("titleCase")
})



#' @rdname annotables
#' @inheritParams all_generics
#' @export
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})
