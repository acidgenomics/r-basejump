#' Make sample data
#'
#' Utility function that prepares metadata to be slotted into
#' [`colData()`][SummarizedExperiment::colData].
#'
#' This function adheres to the following conventions:
#'
#' - All column names will be converted to lower camel case
#'   (see [camelCase()] for details).
#' - Required columns:
#'   - `sampleName`: Human readable sample names. Note that this column is
#'     useful for plots and doesn't have to match the column names of a
#'     `SummarizedExperiment` object, which should use valid names.
#' - Blacklisted columns:
#'   - `filename` (use `fileName`).
#'   - `rowname`.
#'   - `sample`. Too vague. Does this represent an ID or human readable name?
#'   - `samplename` (use `sampleName`).
#' - `sampleName` column is always placed first.
#'
#' @name makeSampleData
#' @note Updated 2019-08-14.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `DataFrame`.
#'
#' @seealso `makeNames`.
#'
#' @examples
#' object <- DataFrame(
#'     genotype = rep(c("control", "wildtype"), times = 2L),
#'     treatment = rep(c("vector", "RNAi"), each = 2L),
#'     sampleName = paste("sample", seq_len(4L)),
#'     row.names = paste0("GSM000000", seq_len(4L))
#' )
#' makeSampleData(object)
NULL



## Updated 2019-08-14.
`makeSampleData,DataFrame` <- function(object) {
    assert(
        hasValidDimnames(object),
        allAreAtomic(object)
    )
    object <- camelCase(object, rownames = FALSE, colnames = TRUE)
    assert(
        ## Don't allow "*Id" columns (note case).
        allAreNotMatchingRegex(x = colnames(object), pattern = "Id$"),
        ## Check for blacklisted columns.
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
    if (!isSubset("sampleName", colnames(object))) {
        object[["sampleName"]] <- rownames(object)
    }
    list <- lapply(
        X = object,
        FUN = function(x) {
            x <- as.factor(x)
            x <- droplevels(x)
            x
        }
    )
    DataFrame(list, row.names = rownames(object))
}



#' @rdname makeSampleData
#' @export
setMethod(
    f = "makeSampleData",
    signature = signature("DataFrame"),
    definition = `makeSampleData,DataFrame`
)
