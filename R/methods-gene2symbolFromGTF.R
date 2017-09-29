#' @rdname gene2symbolFromGFF
#' @export
setMethod("gene2symbolFromGTF", "character", gene2symbolFromGFF)



#' @rdname gene2symbolFromGFF
#' @export
setMethod("gene2symbolFromGTF", "data.frame", gene2symbolFromGFF)
