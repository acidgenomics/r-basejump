#' Unsplit a split data frame
#'
#' Unsplit a `SplitDataFrame` object.
#'
#' Note that the grouping `factor` defined in `f` must be named with the row
#' names of the original `DataFrame`.
#'
#' @name unsplit
#' @note Updated 2019-08-23.
#'
#' @param value `SplitDataFrame`.
#' @param f,drop Intentionally not supported.
#'
#' @return `DataFrame`.
#'
#' @seealso
#' - `help(topic = "DataFrameList", package = "IRanges")`
#' - https://support.bioconductor.org/p/124076/
#' - `BiocGenerics::unsplit()`.
#' - `dplyr::ungroup()`.
#'
#' ```
#' getMethod(
#'     f = "unsplit",
#'     signature = "List",
#'     where = asNamespace("IRanges")
#' )
#' ```
#'
#' ```
#' getMethod(
#'     f = "stack",
#'     signature = "DataFrameList",
#'     where = asNamespace("IRanges")
#' )
#' ```
#'
#' @examples
#' df <- DataFrame(
#'     a = seq_len(4L),
#'     b = as.factor(rep(c("b", "a"), each = 2L)),
#'     row.names = LETTERS[seq_len(4L)]
#' )
#' print(df)
#'
#' f <- df[["b"]]
#' names(f) <- rownames(df)
#' print(f)
#'
#' split <- split(x = df, f = df[["b"]])
#' print(split)
#'
#' unsplit <- unsplit(split)
#' print(unsplit)
NULL



#' @rdname unsplit
#' @name unsplit
#' @importFrom IRanges unsplit
#' @usage unsplit(value, f, drop = FALSE)
#' @export
NULL



## Updated 2019-08-23.
`unsplit,SplitDataFrameList,factor` <-  #
    function(value, f, drop = FALSE) {
        assert(
            hasNames(f),
            identical(drop, FALSE)
        )
        idxCol <- ".idx"
        x <- stack(x = value, index.var = idxCol)
        assert(
            identical(levels(f), levels(x[[idxCol]])),
            areSetEqual(names(f), rownames(x))
        )
        x <- x[names(f), setdiff(colnames(x), idxCol), drop = FALSE]
        x
    }



#' @rdname unsplit
#' @export
setMethod(
    f = "unsplit",
    signature = signature(
        value = "SplitDataFrameList",
        f = "factor"
    ),
    definition = `unsplit,SplitDataFrameList,factor`
)
