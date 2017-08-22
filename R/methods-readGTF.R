#' Read GTF Annotations
#'
#' @rdname readGTF
#' @name readGTF
#'
#' @seealso http://www.gencodegenes.org/gencodeformat.html
#'
#' @examples
#' file.path("http://steinbaugh.com"
#'           "basejump",
#'           "tests",
#'           "mmusculus.gtf") %>%
#'     readGTF %>%
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
        warning = function(w) {
            stop("GTF file failed to load. Check path.", call. = FALSE)
        })

    # Integrity checks
    if (dim(gtf)[[2L]] != 9L) {
        stop("GTF object must be data.frame with 9 columns")
    }

    gtf
})
