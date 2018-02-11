#' Convert to a Unique Character String
#'
#' @rdname toStringUnique
#' @name toStringUnique
#'
#' @inheritParams general
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
#' @importFrom stats na.omit
.toStringUnique <- function(object) {
    object %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}



# Methods ======================================================================
#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("factor"),
    .toStringUnique)



#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("vector"),
    .toStringUnique)
