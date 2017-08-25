#' Markdown Header
#'
#' @rdname mdHeader
#' @name mdHeader
#'
#' @param level Header level (1-7).
#' @param tabset Include tabset marker.
#'
#' @return [writeLines()].
#' @export
#'
#' @examples
#' mdHeader("Header")
#' mdHeader("Header", level = 4L)
#' mdHeader("Header", tabset = TRUE)
NULL



# Methods ====
#' @rdname mdHeader
#' @export
setMethod("mdHeader", "character", function(
    object,
    level = 2L,
    tabset = FALSE) {
    header <- object
    if (!level %in% seq(1L:7L)) {
        stop("Markdown supports 1-7 header levels")
    }
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }
    writeLines(c(
        "",
        "",
        paste(str_dup("#", level), header),
        ""))
})
