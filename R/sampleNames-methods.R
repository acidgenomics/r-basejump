#' Sample names
#'
#' This function will return the human readable sample names if defined
#' in the `sampleName` column of `sampleData`. Otherwise it will return
#' the syntactically valid names defined as the rownames of `sampleData`.
#'
#' @name sampleNames
#' @note Updated 2020-03-15.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Named `character`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' object <- rse
#' x <- sampleNames(object)
#' print(x)
#'
#' ## Assignment support.
#' value <- sampleNames(object)
#' value <- toupper(value)
#' print(value)
#' sampleNames(object) <- value
#' x <- sampleNames(object)
#' print(x)
NULL



## Updated 2019-07-22.
`sampleNames,SummarizedExperiment` <-  # nolint
    function(object) {
        data <- sampleData(object)
        assert(isSubset("sampleName", colnames(data)))
        out <- as.character(data[["sampleName"]])
        names(out) <- rownames(data)
        out
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleNames",
    signature = signature("SummarizedExperiment"),
    definition = `sampleNames,SummarizedExperiment`
)



## Updated 2019-07-22.
`sampleNames<-,SummarizedExperiment,character` <-  # nolint
    function(object, value) {
        if (!is.factor(value)) {
            value <- as.factor(value)
        }
        assert(hasNames(value))
        ## Note that these will correspond to columns for bulk RNA-seq but not
        ## single-cell RNA-seq samples, which map to cells.
        ids <- names(sampleNames(object))
        assert(hasLength(ids))
        ## Require the input to match the original IDs.
        assert(areSetEqual(names(value), ids))
        ## Now safe to reorder the value vector to match.
        value <- value[ids]
        ## Check that the slotting destination matches.
        assert(identical(names(value), rownames(sampleData(object))))
        sampleData(object)[["sampleName"]] <- value
        validObject(object)
        object
    }



#' @rdname sampleNames
#' @export
setMethod(
    f = "sampleNames<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = `sampleNames<-,SummarizedExperiment,character`
)
