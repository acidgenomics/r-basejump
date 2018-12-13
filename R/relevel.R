#' Relevel Row or Column Data
#'
#' @name relevel
#' @inheritParams params
#'
#' @return Modified object.
#' Factor levels will be readjusted (i.e. superfluous levels are dropped).
#'
#' @examples
#' data(rse)
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
relevelRowRanges <- function(rowRanges) {
    message("Releveling factors in rowRanges.")
    mcols <- mcols(rowRanges)
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
    mcols(rowRanges) <- mcols
    rowRanges
}



#' @rdname relevel
#' @export
relevelColData <- function(colData) {
    message("Releveling factors in colData.")
    DataFrame(
        lapply(
            X = colData,
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
        row.names = rownames(colData)
    )
}
