#' @rdname toString
#'
#' @import dplyr
#'
#' @param data Data with rows and columns (e.g. data frame, matrix)
#'
#' @return Summarized data frame that has been collapsed to a single
#'   \code{toString()}-formatted row, separated by commas
#' @export
#'
#' @examples
#' toStringSummarize(head(iris))
toStringSummarize <- function(data) {
    data %>%
        as.data.frame %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
