#' Convert to a Unique Character String
#'
#' @rdname toStringUnique
#' @name toStringUnique
#'
#' @inheritParams AllGenerics
#'
#' @seealso [base::toString()].
#'
#' @return string.
#'
#' @examples
#' vec <- c("hello", "world", NA, "hello", "world", NA)
#' toStringUnique(vec)
NULL



# Methods ======================================================================
#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("character"),
    function(object) {
        object %>%
            na.omit() %>%
            unique() %>%
            toString()
    })
