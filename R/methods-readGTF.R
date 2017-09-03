#' Read GTF Annotations
#'
#' @rdname readGTF
#' @name readGTF
#'
#' @seealso http://www.gencodegenes.org/gencodeformat.html
#'
#' @examples
#' file.path(testDataURL, "mmusculus.gtf") %>%
#'     readGTF %>%
#'     glimpse
NULL



# Methods ====
#' @rdname readGTF
#' @export
setMethod("readGTF", "character", function(object) {
    file <- .localOrRemoteFile(object)
    message(paste("Reading GTF:", names(file)))
    gtf <- tryCatch(
        read.delim(
            file,
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
            header = FALSE),
        error = function(e) {
            stop("GTF file failed to load", call. = FALSE)
        },
        warning = function(w) {
            stop("GTF file failed to load", call. = FALSE)
        })
    gtf
})
