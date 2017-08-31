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
        stop("Markdown supports 1-7 header levels", call. = FALSE)
    }
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }
    header %>%
        # Add the header level
        paste(str_dup("#", level), .) %>%
        # Ensure line breaks
        paste0("\n", ., "\n") %>%
        # Specify that output should be handled as Markdown text
        structure(format = "markdown") %>%
        asis_output
})
