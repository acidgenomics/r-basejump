#' Remove dashes in column and row names
#'
#' This function removes dashes, which can cause unwanted wrapping in RMarkdown reports.
#' @export
#' @importFrom DT datatable
#' @param df \code{data.frame}
#' @param ... Passthrough to \code{datatable} function
sanitizeNames <- function(df, ...) {
    DT::datatable(df, ...,
                  rownames = gsub("-", "_", rownames(df)),
                  colnames = gsub("-", "_", colnames(df)))
}
