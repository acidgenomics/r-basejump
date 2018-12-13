#' Encode Column Data Using Run-Length Encoding
#'
#' @name encode
#' @inheritParams params
#'
#' @seealso `S4Vectors::Rle`.
#'
#' @return Modified object.
#' All `atomic` columns will be encoded to `Rle` S4 class.
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
        DataFrame(
            lapply(
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
            ),
            row.names = rownames(x)
        )
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature("DataFrame"),
    definition = encode.DataFrame
)
