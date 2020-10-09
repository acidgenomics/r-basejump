#' Reexport S4 coercion methods
#'
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @note Updated 2019-08-15.
#' @noRd
NULL



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
        where = "pipette"
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
        where = "pipette"
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
        where = "pipette"
    )
)



## coerce-data.frame-S4methods.R ===============================================
setAs(
    from = "IRanges",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IRanges",
            to = "data.frame"
        ),
        where = "pipette"
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
        where = "pipette"
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
        where = "pipette"
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
        where = "pipette"
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
        where = "pipette"
    )
)

setAs(
    from = "IRanges",
    to = "data.table",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IRanges",
            to = "data.table"
        ),
        where = "pipette"
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
        where = "pipette"
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
        where = "pipette"
    )
)

setAs(
    from = "IRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IRanges",
            to = "tbl_df"
        ),
        where = "pipette"
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
        where = "pipette"
    )
)
