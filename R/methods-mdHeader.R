#' Markdown Header
#'
#' @rdname mdHeader
#' @name mdHeader
#'
#' @param level Header level (1-7).
#' @param tabset Include tabset marker.
#' @param asis Set this to `TRUE` when using the function inside a loop or
#'   inside an RMarkdown chunk with `results="asis"` enabled.
#'
#' @return [writeLines()].
#' @export
#'
#' @examples
#' mdHeader("Header")
#' mdHeader("Header", level = 4L)
#' mdHeader("Header", tabset = TRUE)
#' mdHeader("Header", asis = TRUE)
NULL



# Methods ====
#' @rdname mdHeader
#' @export
setMethod("mdHeader", "character", function(
    object,
    level = 2L,
    tabset = FALSE,
    asis = FALSE) {
    header <- object
    if (!level %in% seq(1L:7L)) {
        stop("Markdown supports 1-7 header levels", call. = FALSE)
    }
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }
    # Add the header level
    header <- paste(str_dup("#", level), header)
    if (isTRUE(asis)) {
        writeLines(c("", "", header, ""))
    } else {
        header %>%
            # Ensure trailing line break
            paste0("\n") %>%
            # Specify that output should be handled as Markdown text
            structure(format = "markdown") %>%
            asis_output
    }
})
