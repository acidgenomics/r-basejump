#' Relevel row or column data
#'
#' @name relevel
#' @inheritParams params
#'
#' @return Modified object.
#' Factor levels will be readjusted (i.e. superfluous levels are dropped).
#'
#' @examples
#' data(rse, package = "acidData")
#'
#' rowRanges <- SummarizedExperiment::rowRanges(rse)
#' x <- relevelRowRanges(rowRanges)
#' summary(x)
#'
#' colData <- SummarizedExperiment::colData(rse)
#' x <- relevelColData(colData)
#' summary(x)
NULL



#' @rdname relevel
#' @export
relevelRowRanges <- function(object) {
    assert(is(object, "GRanges"))
    message("Releveling factors in rowRanges.")
    mcols <- mcols(object)
    mcols <- DataFrame(lapply(
        X = mcols,
        FUN = function(x) {
            if (is.factor(x)) {
                droplevels(x)
            } else if (is(x, "Rle")) {
                x <- decode(x)
                if (is.factor(x)) {
                    x <- droplevels(x)
                }
                Rle(x)
            } else {
                I(x)
            }
        }
    ))
    mcols(object) <- mcols
    object
}



#' @rdname relevel
#' @export
relevelColData <- function(object) {
    assert(is(object, "DataFrame"))
    message("Releveling factors in colData.")
    DataFrame(
        lapply(
            X = object,
            FUN = function(x) {
                if (is.factor(x)) {
                    droplevels(x)
                } else if (is(x, "Rle")) {
                    x <- decode(x)
                    if (is.factor(x)) {
                        x <- droplevels(x)
                    }
                    Rle(x)
                } else {
                    I(x)
                }
            }
        ),
        row.names = rownames(object)
    )
}
