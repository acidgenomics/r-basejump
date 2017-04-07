#' @rdname setNames
#'
#' @import tibble
#'
#' @param column Column to use for \code{rownames}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setRownames(data, column = "ensembl_gene_id")
#' }
setRownames <- function(data, column) {
    # Setting rownames on a tibble is deprecated. Therefore, we must coerce to
    # data frame first, if necessary.
    if (tibble::is_tibble(data)) {
        data <- as.data.frame(data)
    }
    rownames(data) <- data[[column]]
    return(data)
}
