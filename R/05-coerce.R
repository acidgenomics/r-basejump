#' Coercion methods
#'
#' @name coerce
#' @note Updated 2022-03-03.
#'
#' @importFrom methods getMethod setAs signature
#'
#' @details
#' These conversion methods are primarily intended to interconvert between
#' popular tabular formats in R, including `data.frame`, `data.table`, `tbl_df`,
#' and the Bioconductor `DataFrame` classes.
#'
#' @return Modified object, of desired conversion class.
NULL



## Ensure that these remain current with pipette `coerce-setAs-methods.R`.

#' @rdname coerce
#' @name coerce,DataFrame,data.table-method
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

#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
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

#' @rdname coerce
#' @name coerce,GenomicRanges,data.table-method
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

#' @rdname coerce
#' @name coerce,GenomicRanges,tbl_df-method
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

#' @rdname coerce
#' @name coerce,IntegerRanges,data.frame-method
setAs(
    from = "IntegerRanges",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IntegerRanges",
            to = "data.frame"
        ),
        where = "pipette"
    )
)

#' @rdname coerce
#' @name coerce,IntegerRanges,data.table-method
setAs(
    from = "IntegerRanges",
    to = "data.table",
    def = signature(
        from = "IntegerRanges",
        to = "data.table"
    )
)

#' @rdname coerce
#' @name coerce,IntegerRanges,tbl_df-method
setAs(
    from = "IntegerRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "IntegerRanges",
            to = "data.table"
        ),
        where = "pipette"
    )
)

#' @rdname coerce
#' @name coerce,Matrix,DataFrame-method
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

#' @rdname coerce
#' @name coerce,Matrix,data.frame-method
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

#' @rdname coerce
#' @name coerce,data.frame,data.table-method
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

#' @rdname coerce
#' @name coerce,data.frame,tbl_df-method
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

#' @rdname coerce
#' @name coerce,data.table,DataFrame-method
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

#' @rdname coerce
#' @name coerce,tbl_df,DataFrame-method
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
