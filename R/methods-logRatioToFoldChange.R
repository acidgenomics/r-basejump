# Constructors =================================================================
.logRatioToFoldChange <- function(object, base = 2L) {
    assert_is_integer(base)
    assert_is_scalar(base)
    assert_all_are_positive(base)
    object <- base ^ object
    object <- ifelse(object < 1L, -1L / object, object)
    object
}



# Methods ======================================================================
#' @rdname logRatio
#' @export
setMethod(
    "logRatioToFoldChange",
    signature("integer"),
    .logRatioToFoldChange
)



#' @rdname logRatio
#' @export
setMethod(
    "logRatioToFoldChange",
    signature("numeric"),
    .logRatioToFoldChange
)
