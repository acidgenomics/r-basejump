## Updated 2019-07-22.

#' @importFrom methods coerce
#' @exportMethod coerce
NULL



#' @importFrom transformer as.SummarizedExperiment
#' @export
transformer::as.SummarizedExperiment

#' @rdname reexports-S4
#' @name as.data.frame
#' @importFrom transformer as.data.frame
#' @param row.names,optional
#'   Refer to base [`as.data.frame()`][base::as.data.frame] for details.
#' @usage as.data.frame(x, row.names = NULL, optional = FALSE, ...)
#' @export
NULL

#' @importFrom transformer as.data.table
#' @export
transformer::as.data.table

#' @importFrom transformer as_tibble
#' @export
transformer::as_tibble

#' @rdname reexports-S4
#' @name atomize
#' @importFrom transformer atomize
#' @usage atomize(object, ...)
#' @export
NULL

#' @importFrom transformer coerceS4ToList
#' @export
transformer::coerceS4ToList

#' @importFrom transformer left_join
#' @export
transformer::left_join

#' @rdname reexports-S4
#' @name decode
#' @importFrom transformer decode
#' @usage decode(x, ...)
#' @export
NULL

#' @rdname reexports-S4
#' @name encode
#' @importFrom transformer encode
#' @usage encode(x, ...)
#' @export
NULL

#' @rdname reexports-S4
#' @name factorize
#' @importFrom transformer factorize
#' @usage factorize(object, ...)
#' @export
NULL

#' @rdname reexports-S4
#' @name flatFiles
#' @importFrom transformer flatFiles
#' @usage flatFiles(object, ...)
#' @export
NULL

#' @importFrom transformer relevel
#' @export
transformer::relevel



## coerce-DataFrame-S4methods.R ================================================
setAs(
    from = "Matrix",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "Matrix",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "data.table",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "data.table",
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



## coerce-data.frame-S4methods.R ===============================================
setAs(
    from = "IPosRanges",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IPosRanges",
            to = "data.frame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "Matrix",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "Matrix",
            to = "data.frame"
        ),
        where = "transformer"
    )
)



## coerce-data.table-S4methods.R ===============================================
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
    from = "GenomicRanges",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GenomicRanges",
            to = "data.table"
        ),
        where = "transformer"
    )
)

setAs(
    from = "IPosRanges",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IPosRanges",
            to = "data.table"
        ),
        where = "transformer"
    )
)



## coerce-tbl_df-S4methods.R ===================================================
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
    from = "GenomicRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GenomicRanges",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "IPosRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IPosRanges",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

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
