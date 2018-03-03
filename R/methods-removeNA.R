#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @rdname removeNA
#' @name removeNA
#' @family Sanitization Functions
#'
#' @inheritParams general
#'
#' @return Sanitized data.
#'
#' @examples
#' # Remove NA only rows and columns
#' matrix(
#'     c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' ) %>%
#'     removeNA()
#'
#' data.frame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D"),
#'     stringsAsFactors = FALSE
#' ) %>%
#'     removeNA()
#'
#' tibble(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' ) %>%
#'     removeNA()
#'
#' # Support for vectors
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
NULL



# Constructors =================================================================
#' @importFrom stats na.omit
.removeNA.vector <- function(object) {  # nolint
    assert_is_vector(object)
    na.omit(object)
}

.removeNA.dim <- function(object) {  # nolint
    assert_has_dims(object)
    object %>%
        # Remove all `NA` rows
        .[apply(., 1L, function(a) !all(is.na(a))), , drop = FALSE] %>%
        # Remove all `NA` columns
        .[, apply(., 2L, function(a) !all(is.na(a))), drop = FALSE]
}



# Methods ======================================================================
#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("ANY"),
    function(object) {
        # Return unmodified by default
        object
    }
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("character"),
    .removeNA.vector
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("numeric"),
    .removeNA.vector
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("matrix"),
    .removeNA.dim
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("data.frame"),
    .removeNA.dim
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("DataFrame"),
    .removeNA.dim
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("tbl_df"),
    .removeNA.dim
)
