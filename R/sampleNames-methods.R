#' Sample Names
#'
#' This function will return the human readable sample names if defined
#' in the `sampleName` column of [sampleData()]. Otherwise it will return
#' the syntactically valid names defined as the rownames of [sampleData()].
#'
#' @name sampleNames
#' @family Data Functions
#' @author Michael Steinbaugh
#' @importFrom Biobase sampleNames sampleNames<-
#' @export
#'
#' @inheritParams general
#'
#' @return Named `character` vector of the sample names.
#'
#' @examples
#' # SummarizedExperiment ====
#' object <- rse_small
#' x <- sampleNames(object)
#' print(x)
#'
#' # Assignment support
#' value <- sampleNames(object)
#' value <- toupper(value)
#' print(value)
#' sampleNames(object) <- value
#' x <- sampleNames(object)
#' print(x)
#' colData(object)$sampleName
NULL



.sampleNames.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- sampleData(object)
        assert_is_subset("sampleName", colnames(data))
        data <- data[sort(rownames(data)), , drop = FALSE]
        vec <- as.character(data[, "sampleName", drop = TRUE])
        names(vec) <- rownames(data)
        vec
    }



`.sampleNames<-.SE` <-  # nolint
    function(object, value) {
        assert_has_names(value)
        # Note that these will correspond to columns for bulk RNA-seq but not
        # single-cell RNA-seq samples, which map to cells.
        ids <- names(sampleNames(object))
        assert_is_non_empty(ids)
        # Require the input to match the original IDs.
        assert_are_set_equal(names(value), ids)
        # Now safe to reorder the value vector to match.
        value <- value[ids]
        # Check that the slotting destination matches.
        assert_are_identical(names(value), rownames(sampleData(object)))
        sampleData(object)[["sampleName"]] <- value
        validObject(object)
        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleNames",
    signature = signature("SummarizedExperiment"),
    definition = .sampleNames.SE
)



#' @rdname sampleNames
#' @export
setMethod(
    f = "sampleNames<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = `.sampleNames<-.SE`
)
