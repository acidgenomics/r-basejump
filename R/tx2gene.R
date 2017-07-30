#' Convert Ensembl Transcripts to Genes
#'
#' @rdname tx2gene
#'
#' @return Same class as original object.
#' @export
#'
#' @examples
#' # character
#' tx2gene(c("ENSMUST00000000001",
#'           "ENSMUST00000000003",
#'           "ENSMUST00000114041"))
setMethod("tx2gene", "character", function(object) {
    organism <- detectOrganism(object[[1L]])
    t2g <- annotable(organism, format = "tx2gene")
    if (!all(object %in% rownames(t2g))) {
        stop("Unknown transcripts present", call. = FALSE)
    }
    t2g[object, "ensgene"]
})
