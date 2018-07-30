# Methods ======================================================================
#' @rdname logRatio
#' @export
setMethod(
    "logRatioToFoldChange",
    signature("numeric"),
    function(object, base = 2L) {
        assertIsAnImplicitInteger(base)
        base <- as.integer(base)
        assert_all_are_positive(base)
        object <- base ^ object
        object <- ifelse(object < 1L, -1L / object, object)
        object
    }
)
