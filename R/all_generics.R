#' S4 Generics
#'
#' @rdname all_generics
#' @name all_generics
#' @keywords internal
#'
#' @param object Object.
#' @param from Object to coerce.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... Additional arguments.
NULL



#' @rdname annotables
#' @inherit all_generics
#' @export
setGeneric("annotable", function(object) {
    standardGeneric("annotable")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("arrange", function(object, ...) {
    standardGeneric("arrange")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("as_tibble", function(from) {
    standardGeneric("as_tibble")
})



#' @rdname names
#' @inherit all_generics
#' @export
setGeneric("camel", function(object, ...) {
    standardGeneric("camel")
})



#' @rdname names
#' @inherit all_generics
#' @export
setGeneric("dotted", function(object, ...) {
    standardGeneric("dotted")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("filter", function(object, ...) {
    standardGeneric("filter")
})



#' @rdname names
#' @inherit all_generics
#' @export
setGeneric("firstCase", function(object, ...) {
    standardGeneric("firstCase")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("gene2entrez", function(object) {
    standardGeneric("gene2entrez")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("gene2symbol", function(object) {
    standardGeneric("gene2symbol")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("mutate", function(object, ...) {
    standardGeneric("mutate")
})



#' @rdname names
#' @inherit all_generics
#' @export
setGeneric("snake", function(object, ...) {
    standardGeneric("snake")
})



#' @rdname names
#' @inherit all_generics
#' @export
setGeneric("titleCase", function(object, ...) {
    standardGeneric("titleCase")
})



#' @rdname tidy
#' @inherit all_generics
#' @export
setGeneric("tx2gene", function(object) {
    standardGeneric("tx2gene")
})
