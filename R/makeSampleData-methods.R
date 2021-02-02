#' @name makeSampleData
#' @inherit AcidGenerics::makeSampleData
#' @note Updated 2021-02-02.
#'
#' Utility function that prepares metadata to be slotted into
#' [`colData()`][SummarizedExperiment::colData].
#'
#' This function adheres to the following conventions:
#'
#' - Row names are required. Either define manually (recommended) or pass in as
#'   a rownames column (data.table / tibble style).
#'   Supported colnames: "sampleId", "rowname", "rn".
#' - All column names will be converted to lower camel case
#'   (see [camelCase()] for details).
#' - `sampleName` column is always placed first.
#'
#' Required columns:
#'   - `sampleName`: Human readable sample names. Note that this column is
#'     useful for plots and doesn't have to match the column names of a
#'     `SummarizedExperiment` object, which should use valid names.
#'
#' Blacklisted columns:
#'   - `filename` (use `fileName`).
#'   - `id`.
#'   - `interestingGroups`. Defined automatically downstream.
#'   - `sample`. Too vague. Does this represent an ID or human readable name?
#'   - `samplename` (use `sampleName`).
#'
#' @inheritParams AcidRoxygen::params
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



## Updated 2019-08-19.
`makeSampleData,data.frame` <-  # nolint
    function(object) {
        object <- as(object, "DataFrame")
        makeSampleData(object)
    }



#' @rdname makeSampleData
#' @export
setMethod(
    f = "makeSampleData",
    signature = signature("data.frame"),
    definition = `makeSampleData,data.frame`
)



## Updated 2021-01-14.
`makeSampleData,DataFrame` <-  # nolint
    function(object) {
        ## Check for complex S4 columns, which are discouraged.
        assert(
            allAreAtomic(object),
            hasColnames(object)
        )
        ## Enforcing strict as of 2021-01-14.
        object <- camelCase(
            object = object,
            rownames = FALSE,
            colnames = TRUE,
            strict = TRUE
        )
        object <- removeNA(object)
        ## Assign row names from column automatically, if applicable.
        if (!hasRownames(object)) {
            rnCols <- c("sampleId", "rowname", "rn")
            idCol <- as.integer(na.omit(match(
                x = rnCols,
                table = colnames(object)
            )))
            if (isInt(idCol)) {
                ## Don't delete the original ID column here. This is helpful for
                ## FASTQ data provenance with files containing hyphens and other
                ## invalid characters.
                rownames(object) <- makeNames(object[[idCol]], unique = TRUE)
            }
        }
        assert(
            hasRownames(object),
            ## Check for blacklisted columns.
            areDisjointSets(
                x = c(
                    ## rn,
                    ## rowname,
                    ## sampleId,
                    "filename",
                    "id",
                    "interestingGroups",
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
        out <- DataFrame(list, row.names = rownames(object))
        ## Ensure rownames are sorted and `sampleName` column is always first.
        assert(hasRownames(out), hasColnames(out))
        out <- out[
            sort(rownames(out)),
            unique(c("sampleName", colnames(out))),
            drop = FALSE
        ]
        out
    }



#' @rdname makeSampleData
#' @export
setMethod(
    f = "makeSampleData",
    signature = signature("DataFrame"),
    definition = `makeSampleData,DataFrame`
)
