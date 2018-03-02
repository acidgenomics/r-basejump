#' Markdown List
#'
#' @rdname markdownList
#' @name markdownList
#' @family R Markdown Functions
#'
#' @inherit markdownHeader
#'
#' @param ordered Ordered (`TRUE`; `1.`) or unordered (`FALSE`; `-`) list in
#'   Markdown format.
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' markdownList(groceries)
#' markdownList(groceries, ordered = TRUE)
#' markdownList(groceries, asis = TRUE)
NULL



# Methods ======================================================================
#' @rdname markdownList
#' @importFrom knitr asis_output
#' @export
setMethod(
    "markdownList",
    signature("character"),
    function(
        object,
        ordered = FALSE,
        asis = FALSE) {
        assert_is_character(object)
        assert_all_are_not_na(object)
        assert_all_are_non_missing_nor_empty_character(object)
        assert_is_a_bool(ordered)
        assert_is_a_bool(asis)

        list <- vapply(
            X = seq_along(object),
            FUN = function(a) {
                if (isTRUE(ordered)) {
                    prefix <- paste0(a, ".")
                } else {
                    prefix <- "-"
                }
                paste(prefix, object[[a]])
            },
            FUN.VALUE = character(1L)
        )

        if (isTRUE(asis)) {
            writeLines(c("", list, ""))
        } else {
            list %>%
                # Add a trailing line break
                paste0("\n") %>%
                # Specify that output should be handled as Markdown text
                structure(format = "markdown") %>%
                asis_output()
        }
    })



# Aliases ======================================================================
#' @rdname markdownList
#' @inheritParams general
#' @export
mdList <- function(...) {
    markdownList(...)  # nocov
}
