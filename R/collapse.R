#' Collapse Utilities
#'
#' Helper functions for to produce collapsed output for vectors
#' ([toString()]) and data frames ([summarizeRows()]).
#'
#' @rdname collapse
#'
#' @param x The object to be converted.
#' @param sep Separator used for collapse.
#'
#' @return A character vector of length 1 is returned.
#'
#' @examples
#' groceries <- c("milk", "eggs", "eggs", NA)
#'
#' toStringUnique(groceries)
#' toStringSortUnique(groceries)
#'
#' mtcars %>% head %>% summarizeRows



#' @rdname collapse
#' @export
setMethod("toStringUnique", "character", function(object, sep = ", ") {
    object %>%
        str_replace_na %>%
        unique %>%
        str_c(collapse = sep)
})



#' @rdname collapse
#' @export
setMethod("toStringSortUnique", "character", function(object, sep = ", ") {
    object %>%
        str_replace_na %>%
        unique %>%
        sort %>%
        str_c(collapse = sep)
})



# summarizeRows ====
#' @rdname collapse
#' @usage NULL
.summarizeRows <- function(object, sep = ", ") {
    object %>%
        as("tibble") %>%
        summarise_all(funs(toStringSortUnique(., sep = sep))) %>%
        mutate_all(funs(fixNA))
}

#' @rdname collapse
#' @export
setMethod("summarizeRows", "data.frame", .summarizeRows)

#' @rdname collapse
#' @export
setMethod("summarizeRows", "matrix", .summarizeRows)
