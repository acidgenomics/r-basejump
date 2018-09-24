#' Sample Data
#'
#' Metadata that describes the samples.
#'
#' This is a complement to the standard [colData()] function, but improves
#' support for accessing sample metadata for datasets where multiple items in
#' the columns map to a single sample (e.g. cells for a single-cell RNA-seq
#' experiment).
#'
#' @name sampleData
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- rse_small
#' sampleData(x)
#'
#' # Assignment support
#' sampleData(x)[["batch"]] <- 1L
#' # `batch` column should be now defined.
#' sampleData(x)
#'
#' # SingleCellExperiment ====
#' x <- sce_small
#' sampleData(x)
#'
#' # Assignment support.
#' sampleData(x)[["batch"]] <- 1L
#' # `batch` column should be now defined.
#' sampleData(x)
NULL



.sampleData.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- colData(object)
        assertHasRownames(data)

        # Require `sampleName` column to be defined.
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- as.factor(rownames(data))
        }

        # Generate `interestingGroups` column.
        data[["interestingGroups"]] <- NULL
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        data
    }



`.sampleData<-.SE` <-  # nolint
    function(object, value) {
        # Don't allow blacklisted columns.
        value[["interestingGroups"]] <- NULL
        value[["rowname"]] <- NULL
        value[["sampleID"]] <- NULL
        # Now safe to assign and return.
        colData(object) <- value
        validObject(object)
        object
    }



.sampleData.SCE <-  # nolint
    function(object) {
        data <- colData(object)

        # Require `sampleID` and `sampleName` columns.
        # Note that `SummarizedExperiment` method differs but not requiring
        # the `sampleID` column, which are the `colnames` of the object.
        # `SingleCellExperiment` maps cells to `colnames` instead of samples.
        if (!all(c("sampleID", "sampleName") %in% colnames(data))) {
            stop(paste(
                "`sampleData()` requires `sampleID` and `sampleName` columns",
                "to be defined in `colData()`"
            ), call. = FALSE)
        }

        # Get the number of samples.
        nSamples <- length(unique(data[["sampleID"]]))
        assert_all_are_positive(nSamples)

        # Select only columns that map to samples.
        isSampleLevel <- vapply(
            X = data,
            FUN = function(x) {
                uniques <- length(unique(x))
                uniques <= nSamples
            },
            FUN.VALUE = logical(1L)
        )
        data <- data[, isSampleLevel, drop = FALSE]

        # `clusterCols` used to define cluster mappings are blacklisted.
        data <- data[
            ,
            !grepl(
                pattern = paste(clusterCols, collapse = "|"),
                x = camel(colnames(data))
            ),
            drop = FALSE
            ]

        # Collapse and set the rownames to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        assert_has_no_duplicates(data[["sampleID"]])
        rownames(data) <- data[["sampleID"]]
        data[["sampleID"]] <- NULL
        # Return sorted by `sampleID`.
        data <- data[rownames(data), , drop = FALSE]

        # Generate `interestingGroups` column.
        data[["interestingGroups"]] <- NULL
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        data
    }



`.sampleData<-.SCE` <-  # nolint
    function(object, value) {
        # Remove legacy `sampleData` in metadata, if defined.
        if (!is.null(metadata(object)[["sampleData"]])) {
            message("Removing legacy `sampleData` in `metadata()` slot")
            metadata(object)[["sampleData"]] <- NULL
        }

        # Don't allow blacklisted columns.
        value[["interestingGroups"]] <- NULL
        value[["rowname"]] <- NULL
        value[["sampleID"]] <- NULL

        # Generate `sampleID` column.
        value[["sampleID"]] <- as.factor(rownames(value))

        # Update colData slot.
        colData <- colData(object)
        assert_is_subset("sampleID", colnames(colData))
        colData <- colData[
            ,
            c("sampleID", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
        ]
        colData <- left_join(
            x = colData,
            y = value,
            by = "sampleID"
        )
        colData(object) <- colData

        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = .sampleData.SE
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `.sampleData<-.SE`
)




#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SingleCellExperiment"),
    definition = .sampleData.SCE
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SingleCellExperiment",
        value = "DataFrame"
    ),
    definition = `.sampleData<-.SCE`
)
