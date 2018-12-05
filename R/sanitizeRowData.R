#' Sanitize Row Data
#'
#' Coerce row annotations (e.g. genes or transcripts) to `DataFrame`, and keep
#' only `atomic` columns. Complex columns (e.g. Entrez ID `list`) will fail to
#' write to disk as CSVs.
#'
#' @export
#'
#' @param object `DataFrame` or `GRanges`.
#'
#' @return `DataFrame`. Contains only `atomic` columns.
#'
#' @examples
#' data(rse)
#'
#' ## rowData.
#' x <- SummarizedExperiment::rowData(rse)
#' x <- sanitizeRowData(x)
#' vapply(x, is.atomic, logical(1L))
#' print(x)
#'
#' ## rowRanges.
#' x <- SummarizedExperiment::rowRanges(rse)
#' x <- sanitizeRowRanges(x)
#' vapply(x, is.atomic, logical(1L))
#' print(x)
sanitizeRowData <- function(object) {
    assert_that(any(is2(object, class = c("GRanges", "DataFrame"))))
    .atomicDataFrame(object)
}



#' @rdname sanitizeRowData
#' @export
sanitizeRowRanges <- sanitizeRowData
