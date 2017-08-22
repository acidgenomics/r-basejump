#' Read GTF Annotations
#'
#' @rdname readGTF
#' @name readGTF
#'
#' @seealso http://www.gencodegenes.org/gencodeformat.html
#'
#' @examples
#' readGTF("http://steinbaugh.com/basejump/tests/mmusculus.gtf") %>%
#'     glimpse
NULL



# Methods ====
#' @rdname readGTF
#' @export
setMethod("readGTF", "character", function(object) {
    # Check for remote file
    if (str_detect(object, "://")) {
        # Save as temp file
        file <- tempfile()
        download.file(object, file)
    } else {
        file <- object
    }
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
        header = FALSE)
})
