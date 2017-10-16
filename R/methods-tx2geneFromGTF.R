#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGTF",
    signature("character"),
    .tx2geneFromGFF)



#' @rdname tx2geneFromGFF
#' @export
setMethod(
    "tx2geneFromGTF",
    signature("data.frame"),
    .tx2geneFromGFF)
