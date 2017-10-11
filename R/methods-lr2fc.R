#' @rdname logRatio
#' @export
setMethod(
    "lr2fc",
    signature("numeric"),
    function(object, base = 2L) {
        object <- base ^ object
        object <- ifelse(object < 1L, -1L / object, object)
        object
    })
