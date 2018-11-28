# TODO Consider adding SCE method support that works on `sampleData`.



#' Autopad Zeros in Vector
#'
#' @note For methods on objects supporting `dim()` (e.g. `matrix` or
#' `SummarizedExperiment`), the object will be returned with the rows and/or
#' columns resorted by default. This does not apply to the `character` method.
#'
#' @name autopadZeros
#' @inheritParams params
#' @inheritParams makeNames
#'
#' @return `character`.
#'
#' @examples
#' data(rse)
#'
#' ## character ====
#' autopadZeros(c("A1", "B10"))
#' autopadZeros(c("A1", "B10", "C100"))
#'
#' ## SummarizedExperiment ====
#' autopadZeros(rse)
NULL



autopadZeros.character <- function(object) {
    assert_is_character(object)
    assertAreValidNames(object)
    pattern <- "(.*[A-Za-z])([[:digit:]]+)$"
    # Early return if no padding is necessary.
    if (!all(grepl(pattern = pattern, x = object))) {
        return(object)
    }
    match <- str_match(string = object, pattern = pattern)
    prefix <- match[, 2L]
    nums <- match[, 3L]
    width <- max(str_length(nums))
    nums <- str_pad(string = nums, width = width, side = "left", pad = "0")
    mat <- matrix(data = c(prefix, nums), ncol = 2L)
    paste0(mat[, 1L], mat[, 2L])
}



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("character"),
    definition = autopadZeros.character
)



autopadZeros.matrix <- function(
    object,
    rownames = FALSE,
    colnames = TRUE,
    sort = TRUE
) {
    assertHasValidDimnames(object)
    assert_is_a_bool(rownames)
    assert_is_a_bool(colnames)
    assert_is_a_bool(sort)
    if (isTRUE(rownames)) {
        rownames(object) <- autopadZeros(rownames(object))
        if (isTRUE(sort)) {
            object <- object[sort(rownames(object)), , drop = FALSE]
        }
    }
    if (isTRUE(colnames)) {
        colnames(object) <- autopadZeros(colnames(object))
        if (isTRUE(sort)) {
            object <- object[, sort(colnames(object)), drop = FALSE]
        }
    }
    object
}



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("matrix"),
    definition = autopadZeros.matrix
)



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("SummarizedExperiment"),
    definition = autopadZeros.matrix
)
