#' Markdown List
#'
#' Include a Markdown-formatted list, either ordered or unordered. This function
#' works in any R Markdown code block. When calling from inside an `asis` chunk,
#' set `asis = TRUE`.
#'
#' @family Markdown Functions
#' @export
#'
#' @inherit markdownHeader
#'
#' @param ordered `boolean`. Ordered ("`TRUE`") or unordered ("`FALSE`").
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' markdownList(groceries)
#' markdownList(groceries, ordered = TRUE)
#' markdownList(groceries, asis = TRUE)
markdownList <- function(
    text,
    ordered = FALSE,
    asis = FALSE
) {
    assert_is_atomic(text)
    text <- as.character(text)
    assert_all_are_not_na(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assert_is_a_bool(ordered)
    assert_is_a_bool(asis)

    list <- vapply(
        X = seq_along(text),
        FUN = function(a) {
            if (isTRUE(ordered)) {
                prefix <- paste0(a, ".")
            } else {
                prefix <- "-"
            }
            paste(prefix, text[[a]])
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
}



#' @rdname markdownList
#' @export
mdList <- markdownList
