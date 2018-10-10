#' Return the First and Last Part of an Object
#'
#' @name headtail
#' @export
#'
#' @inheritParams general
#' @param n `scalar integer`. Positive integer denoting the number of first and
#'   last items to include.
#'
#' @return `character`.
#'
#' @seealso [head], [tail].
NULL



.headtail.atomic <- function(x, n = 2L) {
    assert_is_atomic(x)
    assert_is_an_integer(n)
    paste(
        c(
            head(x, n = n),
            "...",
            tail(x, n = n)
        ),
        collapse = " "
    )
}



#' @describeIn headtail Paste collapse to a `string`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("atomic"),
    definition = .headtail.atomic
)
