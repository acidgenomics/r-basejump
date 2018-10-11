# FIXME n = 1 errors out here.



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
#' - [S4Vectors::showAsCell].
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
    invisible()
}



# FIXME Improve this function to be less strict for objects with small n.
# FIXME Coerce factors to character.
# FIXME Use `showAsCell()` here to handle "..." better for factors.
.headtail.matrix <- function(x, n = 2L) {
    assert_has_dims(x)
    assertIsAnImplicitInteger(n)
    assert_all_are_positive(n)

    # Consider making this less strict.
    stopifnot(nrow(x) >= n * 2L)
    stopifnot(ncol(x) >= n * 2L)

    square <- x[
        c(
            head(rownames(x), n = n),
            tail(rownames(x), n = n)
        ),
        c(
            head(colnames(x), n = n),
            tail(colnames(x), n = n)
        ),
        drop = FALSE
    ]

    # For sparseMatrix, this step depends on our custom coerion methods.
    # We want to apply this step after subsetting, so a large matrix doesn't
    # deparse and blow up in memory.
    square <- as.data.frame(square)

    # FIXME Coerce factors to strings, to avoid invalid factor level NAs.
    # For example, this can happens with `seqnames` from GRanges.

    # Check that we have square dimensions.
    stopifnot(nrow(square) == n * 2L)
    stopifnot(ncol(square) == n * 2L)

    # Split into quadrants, so we can add vertical separators.
    # upper/lower, left/right.
    ul <- square[seq_len(n), seq_len(n)]
    ur <- square[seq_len(n), seq_len(n) + n]
    ll <- square[seq_len(n) + n, seq_len(n)]
    lr <- square[seq_len(n) + n, seq_len(n) + n]

    # Add horizontal separators between head and tail.
    head <- data.frame(
        ul,
        "..." = rep("...", times = n),
        ur
    )
    tail <- data.frame(
        ll,
        "..." = rep("...", times = n),
        lr
    )
    out <- rbind(
        head,
        "..." = rep("...", times = n * 2L),
        tail
    )

    print(out)
    invisible()
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



# FIXME Improve handling for factor columns. seqnames generates a warning.
#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = function(x, n = 2L) {
        headtail(as(x, "data.frame"), n = n)
    }
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
