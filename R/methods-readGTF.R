#' Read GTF Annotations
#'
#' @rdname readGTF
#' @name readGTF
NULL



# Methods ====
#' @rdname readGTF
#' @export
setMethod("readGTF", "character", function(object) {
    # http://www.gencodegenes.org/gencodeformat.html
    gtf <- read.delim(
        object,
        col.names = c("chromosome",
                      "annotationSource",
                      "featureType",
                      "start",
                      "end",
                      "score",
                      "strand",
                      "phase",
                      "keyValuePairs"),
        comment.char = "#",
        header = FALSE)
    if (dim(gtf)[[2L]] != 9L) {
        stop("GTF object must be data.frame with 9 columns")
    }
    gtf
})
