#' toString Call that Only Outputs Uniques.
#'
#' @rdname toStringUnique
#' @name toStringUnique
#'
#' @inheritParams AllGenerics
#'
#' @return string.
NULL



# Methods ====
#' @rdname toStringUnique
#' @export
setMethod(
    "toStringUnique",
    signature("character"),
    function(object) {
        object %>%
            unique() %>%
            toString() %>%
            gsub(x = .,
                 pattern = "NA,\\s|,\\sNA",
                 replacement = "")
    })
