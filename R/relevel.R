#' Relevel Row or Column Data
#'
#' @name relevel
#' @inheritParams params
#'
#' @export
#'
#' @examples
#' data(rse)
#'
#' rowRanges <- rowRanges(rse)
#' relevelRowRanges(rowRanges)
#'
#' colData <- colData(rse)
#' relevelColData(colData)
NULL



#' @rdname relevel
#' @export
relevelRowRanges <- function(rowRanges) {
    message("Releveling factors in rowRanges.")
    mcols <- mcols(rowRanges)
    mcols <- DataFrame(lapply(
        X = mcols,
        FUN = function(x) {
            if (is(x, "Rle")) {
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
}



# TODO Need to keep this encoded, if necessary.
#' @rdname relevel
#' @export
relevelColData <- function(colData) {
    message("Releveling factors in colData.")
    colData <- colData %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, droplevels) %>%
        column_to_rownames() %>%
        as("DataFrame")
}
