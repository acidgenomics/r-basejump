#' @rdname tx2geneFromGFF
#' @export
setMethod("tx2geneFromGTF", "character", tx2geneFromGFF)



#' @rdname tx2geneFromGFF
#' @export
setMethod("tx2geneFromGTF", "data.frame", tx2geneFromGFF)
