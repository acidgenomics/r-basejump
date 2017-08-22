#' Markdown Header
#'
#' @rdname mdHeader
#' @name mdHeader
#'
#' @param level Header level (1-7).
#'
#' @return [writeLines()].
#' @export
#'
#' @examples
#' mdHeader("Header")
#' mdHeader("Header", level = 4L)
NULL



# Methods ====
#' @rdname mdHeader
#' @export
setMethod("mdHeader", "character", function(object, level = 2L) {
    if (!level %in% seq(1L:7L)) {
        stop("Markdown supports 1-7 header levels")
    }
    writeLines(c(
        "",
        "",
        paste(str_dup("#", level), object),
        ""))
})
