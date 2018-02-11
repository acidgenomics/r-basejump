#' @rdname logRatio
#' @export
setMethod(
    "logRatioToFoldChange",
    signature("numeric"),
    function(object, base = 2L) {
        assert_is_numeric(base)
        object <- base ^ object
        object <- ifelse(object < 1L, -1L / object, object)
        object
    })
