# TODO Check for trailing "Id" and error.
# TODO Check for trailing "name" and error.



#' Make Sample Data
#'
#' Utility function that prepares metadata to be slotted into `colData()`.
#'
#' This function adheres to the following conventions:
#'
#' - All column names will be converted to lower camel case
#'   (see `camel()` for details).
#' - Required columns:
#'   - `sampleName`: Human readable sample names. Note that this column is
#'     useful for plots and doesn't have to match the column names of a
#'     `SummarizedExperiment` object, which should use valid names.
#' - Blacklisted columns:
#'   - `filename` (use `fileName`).
#'   - `rowname`.
#'   - `sample`. Too vague. Does this represent an ID or human readable name?
#'   - `samplename` (use `sampleName`),
#' - `sampleName` column is always placed first.
#'
#' @inheritParams params
#' @export
#'
#' @return `DataFrame`.
#'
#' @seealso `makeNames`.
#'
#' @examples
#' object <- data.frame(
#'     genotype = rep(c("control", "wildtype"), times = 2L),
#'     treatment = rep(c("vector", "RNAi"), each = 2L),
#'     sampleName = paste("sample", seq_len(4L)),
#'     row.names = paste0("GSM000000", seq_len(4L))
#' )
makeSampleData <- function(object) {
    assert(
        hasValidDimnames(object),
        isSuperset(
            x = colnames(object),
            y = "sampleName"
        ),
        areDisjointSets(
            x = c(
                "filename",
                "id",
                "interestingGroups",
                "rowname",
                "sample",
                "samplename"
            ),
            y = colnames(object)
        )
    )
    data <- object %>%
        as_tibble(rownames = "rowname") %>%
        mutate_all(as.factor) %>%
        mutate_all(droplevels) %>%
        select(!!sym("sampleName"), everything()) %>%
        as("DataFrame")
    assert(hasRownames(data))
    data
}
