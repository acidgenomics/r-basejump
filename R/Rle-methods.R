# decode =======================================================================
#' @name decode
#' @inherit S4Vectors::decode title description return
#' @inheritParams params
#' @examples
#' data(rse)
#' mcols <- S4Vectors::mcols(rse)
#' lapply(mcols, class)
#' x <- decode(mcols)
#' lapply(x, class)
NULL



#' @importFrom S4Vectors decode
#' @aliases NULL
#' @export
S4Vectors::decode



decode.DataFrame <- function(x) {
    DataFrame(lapply(
        X = x,
        FUN = function(x) {
            if (is(x, "Rle")) {
                x <- decode(x)
                if (is.factor(x)) {
                    x <- droplevels(x)
                }
                x
            } else if (!is.atomic(x)) {
                I(x)
            } else {
                x
            }
        }
    ))
}



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("DataFrame"),
    definition = decode.DataFrame
)



# encode =======================================================================
#' Encode Column Data Using Run-Length Encoding
#'
#' @name encode
#' @inheritParams params
#'
#' @seealso [S4Vectors::Rle()].
#'
#' @return Modified object, with `atomic` columns converted to `Rle`.
#'
#' @examples
#' binary <- seq(from = 0L, to = 1L)
#' df <- S4Vectors::DataFrame(
#'     a = rep(x = binary, times = 50L),
#'     b = rep(x = binary, each = 50L)
#' )
#' lapply(df, class)
#' x <- encode(df)
#' lapply(x, class)
NULL



encode.DataFrame <-  # nolint
    function(x) {
        DataFrame(lapply(
            X = x,
            FUN = function(x) {
                # Decode Rle, if necessary.
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                # Adjust (drop) factor levels, if necessary.
                if (is.factor(x)) {
                    x <- droplevels(x)
                }
                # Use run-length encoding on atomics.
                if (is.atomic(x)) {
                    Rle(x)
                } else {
                    I(x)
                }
            }
        ))
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature("DataFrame"),
    definition = encode.DataFrame
)
