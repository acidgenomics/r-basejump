#' @name mcolnames
#' @inherit acidgenerics::mcolnames
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## Vector ====
#' object <- RangedSummarizedExperiment
#' mcolnames(object)
#' ## Asignment method.
#' mcolnames(object) <- toupper(mcolnames(object))
#' mcolnames(object)
NULL



#' @rdname mcolnames
#' @name mcolnames
#' @importFrom acidgenerics mcolnames
#' @usage mcolnames(x, ...)
#' @export
NULL

#' @rdname mcolnames
#' @name mcolnames<-
#' @importFrom acidgenerics mcolnames<-
#' @usage mcolnames(x, ...) <- value
#' @export
NULL



## Updated 2019-07-19.
`mcolnames,Vector` <-  # nolint
    function(x) {
        names(mcols(x))
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames",
    signature = signature(x = "Vector"),
    definition = `mcolnames,Vector`
)



## Updated 2019-07-19.
`mcolnames<-,Vector,character` <-  # nolint
    function(x, value) {
        assert(
            isCharacter(value),
            areSameLength(names(mcols(x)), value)
        )
        names(mcols(x)) <- value
        x
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames<-",
    signature = signature(
        x = "Vector",
        value = "character"
    ),
    definition = `mcolnames<-,Vector,character`
)
