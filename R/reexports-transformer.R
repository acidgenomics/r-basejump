# To my knowledge, there isn't an easier way to reexport S4 methods in another
# package, so redefine current supported methods again here.

#' @importFrom methods coerce
#' @exportMethod coerce
NULL



#' @importFrom transformer coerceS4ToList
#' @export
transformer::coerceS4ToList

#' @rdname reexports-S4
#' @name flatFiles
#' @importFrom transformer flatFiles
#' @usage flatFiles(object, ...)
#' @export
NULL




# coerce-DataFrame.R ===========================================================
#' @importFrom transformer as.DataFrame
#' @export
transformer::as.DataFrame

setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "tbl_df",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)



# coerce-SummarizedExperiment.R ================================================
#' @importFrom transformer as.SummarizedExperiment
#' @export
transformer::as.SummarizedExperiment




# coerce-data.frame.R ==========================================================
#' @rdname reexports-S4
#' @name as.data.frame
#' @importFrom transformer as.data.frame
#' @param row.names,optional
#'   Refer to base [`as.data.frame()`][base::as.data.frame] for details.
#' @usage as.data.frame(x, row.names = NULL, optional = FALSE, ...)
#' @export
NULL

setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "data.frame"
        ),
        where = "transformer"
    )
)



# coerce-data.table.R ==========================================================
#' @importFrom transformer as.data.table
#' @export
transformer::as.data.table

setAs(
    from = "data.frame",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "data.frame",
            to = "data.table"
        ),
        where = "transformer"
    )
)

setAs(
    from = "DataFrame",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "DataFrame",
            to = "data.table"
        ),
        where = "transformer"
    )
)

setAs(
    from = "GRanges",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GRanges",
            to = "data.table"
        ),
        where = "transformer"
    )
)



# coerce-tbl_df.R ==============================================================
#' @importFrom transformer as_tibble
#' @export
transformer::as_tibble

setAs(
    from = "data.frame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "data.frame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "DataFrame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "GRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GRanges",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)
