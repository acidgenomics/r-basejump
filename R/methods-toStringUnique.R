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
NULL



# Methods ======================================================================
#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("character"),
    function(object) {
        object %>%
            unique() %>%
            toString() %>%
            gsub("NA,\\s|,\\sNA", "", .)
    })
