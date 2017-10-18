# These generics are currently currently in use for both the bcbioRNASeq and
# bcbioSingleCell packages



#' @rdname AllGenerics
#' @export
setGeneric("aggregateReplicates", function(object, ...) {
    standardGeneric("aggregateReplicates")
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



#' @rdname AllGenerics
#' @export
setGeneric("flatFiles", function(object, ...) {
    standardGeneric("flatFiles")
})



#' @rdname AllGenerics
#' @export
setGeneric("interestingGroups", function(object, ...) {
    standardGeneric("interestingGroups")
})



#' @rdname AllGenerics
#' @export
setGeneric("interestingGroups<-", function(object, ..., value) {
    standardGeneric("interestingGroups<-")
})



#' @rdname AllGenerics
#' @export
setGeneric("metrics", function(object, ...) {
    standardGeneric("metrics")
})



#' @rdname AllGenerics
#' @export
setGeneric("plotGene", function(object, ...) {
    standardGeneric("plotGene")
})



#' @rdname AllGenerics
#' @export
setGeneric("sampleDirs", function(object, ...) {
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
