#' Autopad Zeros in Vector
#'
#' @inheritParams params
#' @export
#'
#' @return `character`.
#'
#' @examples
#' autopadZeros(c("A1", "B10"))
#' autopadZeros(c("A1", "B10", "C100"))
autopadZeros <- function(object) {
    assert_is_character(object)
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
