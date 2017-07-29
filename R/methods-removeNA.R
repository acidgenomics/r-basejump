#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @rdname removeNA
#'
#' @return Sanitized data.
#'
#' @examples
#' # Remove NA only rows and columns
#' matrix(c(1, NA, 3,
#'          NA, NA, NA,
#'          2, NA, 4),
#'        nrow = 3, ncol = 3) %>% removeNA
#'
#' data.frame(a = c("A", NA, "C"),
#'            b = c(NA, NA, NA),
#'            c = c("B", NA, "D")) %>% removeNA
#'
#' tibble(a = c("A", NA, "C"),
#'        b = c(NA, NA, NA),
#'        c = c("B", NA, "D")) %>% removeNA
#'
#'
#' # Support for vectors (using `stats::na.omit()`)
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#'
#' # Return unmodified
#' list(a = c("A", NA, "C"),
#'      b = c(NA, NA, NA),
#'      c = c("B", NA, "D")) %>% removeNA



#' @rdname removeNA
#' @usage NULL
.removeNA <- function(object) {
    object %>%
        .[apply(., 1L, function(a) !all(is.na(a))), ] %>%  # rows
        .[, apply(., 2L, function(a) !all(is.na(a)))]      # cols
}



#' @rdname removeNA
#' @export
setMethod("removeNA", "character", function(object) {
    stats::na.omit(object)
})

#' @rdname removeNA
#' @export
setMethod("removeNA", "numeric", function(object) {
    stats::na.omit(object)
})



#' @rdname removeNA
#' @export
setMethod("removeNA", "matrix", .removeNA)

#' @rdname removeNA
#' @export
setMethod("removeNA", "dgCMatrix", .removeNA)



#' @rdname removeNA
#' @export
setMethod("removeNA", "data.frame", .removeNA)

#' @rdname removeNA
#' @export
setMethod("removeNA", "DataFrame", .removeNA)

#' @rdname removeNA
#' @export
setMethod("removeNA", "tbl_df", .removeNA)
