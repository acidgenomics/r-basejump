#' Convert to a Unique Character String
#'
#' @rdname toStringUnique
#' @name toStringUnique
#'
#' @inheritParams AllGenerics
#'
#' @seealso [base::toString()].
#'
#' @return String.
#'
#' @examples
#' vec <- c("hello", "world", NA, "hello", "world", NA)
#' toStringUnique(vec)
NULL



# Constructors =================================================================
.toStringUnique <- function(object) {
    object %>%
        na.omit() %>%
        unique() %>%
        toString()
}



# Methods ======================================================================
#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("character"),
    .toStringUnique)
