# decode =======================================================================
#' @name decode
#' @inherit S4Vectors::decode title description return
#' @inheritParams params
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
            } else {
                I(x)
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



# Encode =======================================================================
#' Encode Column Data Using Run-Length Encoding
#'
#' @name encode
#' @inheritParams params
#'
#' @seealso [S4Vectors::Rle()].
#'
#' @return Modified object, with `atomic` columns converted to `Rle`.
NULL



encode.DataFrame <-  # nolint
    function(x) {
        DataFrame(lapply(
            X = x,
            FUN = function(x) {
                message("Hello there.")
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
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature("DataFrame"),
    definition = encode.DataFrame
)
