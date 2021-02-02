## FIXME DEFINE THIS IN ACIDBASE.



#' @name sem
#' @inherit AcidGenerics::headtail
#' @note `numeric` method is defined in AcidBase.
#' @note Updated 2020-12-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## AsIs ====
#' ## Useful for dplyr mutate calls.
#' x <- seq(from = 1L, to = 10L, by = 1L)
#' x <- I(x)
#' class(x)
#' x <- sem(x)
#' x
NULL



## Necessary for dplyr mutate calls.
##
## Note that AsIs class requires import of BiocGenerics, so defining here
## instead of AcidBase, to keep NAMESPACE more minimal in that package.
##
## Updated 2020-12-04.
`sem,AsIs` <-  # nolint
    function(x) {
        sem(as.numeric(x))
    }



#' @rdname sem
#' @export
setMethod(
    f = "sem",
    signature = signature("AsIs"),
    definition = `sem,AsIs`
)
