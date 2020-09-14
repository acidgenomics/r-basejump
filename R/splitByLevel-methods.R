#' @name splitByLevel
#' @inherit acidgenerics::splitByLevel
#' @note Updated 2020-09-14.
#'
#' @inheritParams acidroxygen::params
#' @param f `character(1)`.
#'   Factor column name.
#' @param ref `logical(1)`.
#'   Include the reference factor level (i.e. first defined) in each split.
#'   Useful for pairwise contrasts.
#' @param ... Additional arguments.
#'
#' @examples
#' df <- DataFrame(
#'     compound = relevel(
#'         factor(
#'             c(
#'                 rep(x = "dmso", each = 3L),
#'                 rep(x = c("ts_0001", "ts_0002"), each = 9L)
#'             )
#'         ),
#'         ref = "dmso"
#'     ),
#'     concentration = factor(
#'         c(
#'             rep(x = 0, each = 3L),
#'             rep(x = c(0.1, 1, 10), each = 3L, times = 2L)
#'         )
#'     ),
#'     replicate = factor(
#'         rep(
#'             seq(from = 1L, to = 3L),
#'             times = 7L
#'         )
#'     )
#' )
#' lapply(df, levels)
#'
#' splitByLevel(df, f = "compound", ref = FALSE)
#' splitByLevel(df, f = "compound", ref = TRUE)
#'
#' splitByLevel(df, f = "concentration", ref = FALSE)
#' splitByLevel(df, f = "concentration", ref = TRUE)
NULL



#' @rdname splitByLevel
#' @name splitByLevel
#' @importFrom acidgenerics splitByLevel
#' @usage splitByLevel(x, ...)
#' @export
NULL



## Updated 2020-09-14.
`splitByLevel,data.frame` <-  # nolint
    function(x, f, ref = FALSE) {
        assert(
            isString(f),
            isSubset(f, colnames(x)),
            is.factor(x[[f]]),
            isFlag(ref)
        )
        levels <- levels(x[[f]])
        if (isTRUE(ref)) {
            ref <- levels[[1L]]
            levels <- setdiff(levels, ref)
        } else {
            ref <- NULL
        }
        lapply(
            X = levels,
            ref = ref,
            x = x,
            f = f,
            FUN = function(level, ref, x, f) {
                keep <- x[[f]] %in% c(level, ref)
                x[keep, , drop = FALSE]
            }
        )
    }



#' @rdname splitByLevel
#' @export
setMethod(
    f = "splitByLevel",
    signature = signature("data.frame"),
    definition = `splitByLevel,data.frame`
)



## Updated 2020-09-14.
`splitByLevel,DataFrame` <-  # nolint
    `splitByLevel,data.frame`



#' @rdname splitByLevel
#' @export
setMethod(
    f = "splitByLevel",
    signature = signature("DataFrame"),
    definition = `splitByLevel,DataFrame`
)
