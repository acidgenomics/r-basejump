#' S4 generics
#'
#' @rdname all_generics
#'
#' @param object Primary object.
#' @param from Object to coerce.
#' @param ... Additional arguments.



#' @rdname all_generics
#' @export
setGeneric("annotable", function(object) {
    standardGeneric("annotable")
})



#' @rdname all_generics
#' @export
setGeneric("arrange", function(object, ...) {
    standardGeneric("arrange")
})



#' @rdname all_generics
#' @export
setGeneric("as_tibble", function(from) {
    standardGeneric("as_tibble")
})



#' @rdname all_generics
#' @export
setGeneric("filter", function(object, ...) {
    standardGeneric("filter")
})



#' @rdname all_generics
#' @export
setGeneric("gene2entrez", function(object) {
    standardGeneric("gene2entrez")
})



#' @rdname all_generics
#' @export
setGeneric("gene2symbol", function(object) {
    standardGeneric("gene2symbol")
})



#' @rdname all_generics
#' @export
setGeneric("mutate", function(object, ...) {
    standardGeneric("mutate")
})



#' @rdname all_generics
#' @export
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})
