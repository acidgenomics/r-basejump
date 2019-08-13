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
#' @note Updated 2019-08-13.
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



## Updated 2019-08-13.
`makeSampleData,DataFrame` <- function(object) {
    assert(
        ## Check for strings beginning with numbers, containing spaces, hyphens,
        ## or other characters that aren't valid for names in R.
        hasValidDimnames(object),
        ## Don't allow "*Id" columns (note case).
        all(isNotMatchingRegex(x = colnames(object), pattern = "Id$")),
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
    ## FIXME Consider switching to base S4 methods here.
    out <- object %>%
        as_tibble(rownames = "rowname") %>%
        camelCase() %>%
        mutate_all(as.factor) %>%
        mutate_all(droplevels) %>%
        select(!!sym("sampleName"), everything()) %>%
        as("DataFrame")
    assert(
        hasRownames(out),
        hasValidDimnames(out)
    )
    out
}



#' @rdname makeSampleData
#' @export
setMethod(
    f = "makeSampleData",
    signature = signature("DataFrame"),
    definition = `makeSampleData,DataFrame`
)
