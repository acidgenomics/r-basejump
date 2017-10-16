#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGTF",
    signature("character"),
    .gene2symbolFromGFF)



#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGTF",
    signature("data.frame"),
    .gene2symbolFromGFF)
