#' @rdname logRatio
#' @export
setMethod(
    "logRatioToFoldChange",
    signature("numeric"),
    function(object, base = 2L) {
        .checkBase(base)
        object <- base ^ object
        object <- ifelse(object < 1L, -1L / object, object)
        object
    })
