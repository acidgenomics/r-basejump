# TODO Add GRanges support.



#' Return the First and Last Part of an Object
#'
#' Inspired by the [print()] method for `DataFrame` class objects. Applies to
#' both rows and columns, enabling quick inspection during interactive use.
#'
#' @name headtail
#' @export
#'
#' @inheritParams general
#' @param n `scalar integer`. Positive integer denoting the number of first and
#'   last items to include.
#'
#' @return None (invisible `NULL`).
#'
#' @seealso
#' - [head], [tail], [cat].
#' - `getMethod("show", "DataTable")`.
#'
#' @examples
#' data(rse_small, sce_small)
#'
#' ## data.frame ====
#' headtail(datasets::mtcars)
#'
#' ## SummarizedExperiment ====
#' headtail(rse_small)
#' headtail(colData(rse_small))
#' headtail(rowData(rse_small))
#'
#' ## SingleCellExperiment ====
#' headtail(sce_small)
NULL



.headtail.atomic <- function(x, n = 2L) {
    assert_is_atomic(x)
    assertIsAnImplicitInteger(n)
    assert_all_are_positive(n)
    cat(paste(
        c(
            head(x, n = n),
            "...",
            tail(x, n = n)
        ),
        collapse = " "
    ))
}



# FIXME Rework this to include dots, like DataFrame print method.
.headtail.matrix <- function(x, n = 2L) {
    assert_has_dims(x)
    assertIsAnImplicitInteger(n)
    assert_all_are_positive(n)
    out <- x[
        unique(c(
            head(rownames(x), n = n),
            tail(rownames(x), n = n)
        )),
        unique(c(
            head(colnames(x), n = n),
            tail(colnames(x), n = n)
        )),
        drop = FALSE
    ]
    print(out)
}



#' @describeIn headtail Paste collapse to a `string`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("atomic"),
    definition = .headtail.atomic
)



#' @describeIn headtail Show first and last rows.
#' @export
setMethod(
    f = "headtail",
    signature = signature("matrix"),
    definition = .headtail.matrix
)


#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("sparseMatrix"),
    definition = getMethod("headtail", "matrix")
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("data.frame"),
    definition = getMethod("headtail", "matrix")
)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = getMethod("headtail", "data.frame")
)



#' @describeIn headtail Summarize the primary [assay()].
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = function(x, n = 2L) {
        headtail(assay(x), n = n)
    }
)
