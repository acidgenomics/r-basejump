#' @name mcolnames
#' @inherit bioverbs::mcolnames
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(rse, package = "acidtest")
#' mcolnames(rse)
#'
#' ## Asignment method.
#' mcolnames(rse) <- toupper(mcolnames(rse))
#' mcolnames(rse)
NULL



mcolnames.Vector <-  # nolint
    function(x) {
        colnames(mcols(x))
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames",
    signature = signature(x = "Vector"),
    definition = mcolnames.Vector
)



`mcolnames<-.Vector,character` <-  # nolint
    function(x, value) {
        assert(
            isCharacter(value),
            areSameLength(colnames(mcols(x)), value)
        )
        colnames(mcols(x)) <- value
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
    definition = `mcolnames<-.Vector,character`
)
