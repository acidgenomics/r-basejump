#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @inherit S4Vectors::decode description return
#' @inheritParams params
#'
#' @seealso `S4Vectors::decode`.
#'
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



decode.DataFrame <-  # nolint
    function(x) {
        DataFrame(
            lapply(
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
            ),
            row.names = rownames(x)
        )
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("DataFrame"),
    definition = decode.DataFrame
)
